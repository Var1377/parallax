# Complete API Reference with Practical Examples

## Table of Contents

1. [Bumpalo API Reference](#bumpalo-api-reference)
2. [SmallVec API Reference](#smallvec-api-reference)
3. [IndexMap API Reference](#indexmap-api-reference)
4. [Integration Examples](#integration-examples)
5. [Performance Comparison](#performance-comparison)
6. [Common Patterns](#common-patterns)
7. [Error Handling](#error-handling)
8. [Advanced Usage](#advanced-usage)

## Bumpalo API Reference

### Core Types

```rust
use bumpalo::Bump;

// Main arena allocator
pub struct Bump<const MIN_ALIGN: usize = 1> { /* ... */ }

// Error types
pub struct AllocErr;
pub enum AllocOrInitError<E> {
    Alloc(AllocErr),
    Init(E),
}
```

### Construction

```rust
use bumpalo::Bump;

// Basic construction
let bump = Bump::new();                    // Default alignment (1 byte)
let bump = Bump::with_capacity(1024);      // Pre-allocate 1KB

// Fallible construction
let bump = Bump::try_new()?;
let bump = Bump::try_with_capacity(1024)?;

// Custom alignment
type AlignedBump = Bump<8>;                // 8-byte aligned
let bump = AlignedBump::with_min_align();
let bump = AlignedBump::with_min_align_and_capacity(1024);
```

### Basic Allocation

```rust
use bumpalo::Bump;

let bump = Bump::new();

// Allocate by value
let x = bump.alloc(42);                    // &mut i32
let s = bump.alloc("hello".to_string());   // &mut String

// Fallible allocation
let x = bump.try_alloc(42)?;               // Result<&mut i32, AllocErr>

// Allocate with initializer
let x = bump.alloc_with(|| expensive_computation());
let x = bump.try_alloc_with(|| expensive_computation())?;

// Fallible initializer
let x = bump.alloc_try_with(|| fallible_operation())?;
let x = bump.try_alloc_try_with(|| fallible_operation())?;
```

### String Allocation

```rust
use bumpalo::Bump;

let bump = Bump::new();

// Copy string slice
let s = bump.alloc_str("hello world");     // &mut str
let s = bump.try_alloc_str("hello")?;      // Result<&mut str, AllocErr>

// Build string in arena
let mut string = bumpalo::collections::String::new_in(&bump);
string.push_str("Hello, ");
string.push_str("world!");
let s = string.into_bump_str();            // &str
```

### Slice Allocation

```rust
use bumpalo::Bump;

let bump = Bump::new();
let data = [1, 2, 3, 4, 5];

// Copy slice
let slice = bump.alloc_slice_copy(&data);  // &mut [i32]
let slice = bump.try_alloc_slice_copy(&data)?;

// Clone slice (for non-Copy types)
let strings = ["a", "b", "c"];
let slice = bump.alloc_slice_clone(&strings); // &mut [&str]

// Fill slice with function
let slice = bump.alloc_slice_fill_with(5, |i| i * 2); // [0, 2, 4, 6, 8]
let slice = bump.try_alloc_slice_fill_with(5, |i| i * 2)?;

// Fill with value
let slice = bump.alloc_slice_fill_copy(5, 42);        // [42, 42, 42, 42, 42]
let slice = bump.alloc_slice_fill_clone(3, &"hello".to_string());

// Fill with default
let slice = bump.alloc_slice_fill_default::<i32>(5); // [0, 0, 0, 0, 0]

// Fill from iterator
let iter = (0..5).map(|x| x * x);
let slice = bump.alloc_slice_fill_iter(iter);        // [0, 1, 4, 9, 16]

// Fallible fill from iterator
let iter = (0..5).map(|x| Ok(x * x));
let slice = bump.alloc_slice_try_fill_iter(iter)?;
```

### Layout-Based Allocation

```rust
use bumpalo::Bump;
use std::alloc::Layout;

let bump = Bump::new();

// Allocate raw memory
let layout = Layout::new::<i32>();
let ptr = bump.alloc_layout(layout);       // NonNull<u8>
let ptr = bump.try_alloc_layout(layout)?;

// Initialize manually
unsafe {
    std::ptr::write(ptr.as_ptr() as *mut i32, 42);
    let value = &mut *(ptr.as_ptr() as *mut i32);
    assert_eq!(*value, 42);
}
```

### Arena Management

```rust
use bumpalo::Bump;

let mut bump = Bump::new();

// Allocation limits
bump.set_allocation_limit(Some(1024 * 1024)); // 1MB limit
let limit = bump.allocation_limit();           // Some(1048576)

// Memory usage
let allocated = bump.allocated_bytes();        // Total allocated
let capacity = bump.chunk_capacity();          // Current chunk capacity

// Reset arena (keeps first chunk)
bump.reset();

// Iterate over allocated chunks
for chunk in bump.iter_allocated_chunks() {
    println!("Chunk: {:?}", chunk);
}
```

### Collections

```rust
use bumpalo::{Bump, collections::{Vec, String}};

let bump = Bump::new();

// Vector in arena
let mut vec = Vec::new_in(&bump);
vec.push(1);
vec.push(2);
vec.push(3);
let slice = vec.into_bump_slice();         // &[i32]

// String in arena
let mut string = String::new_in(&bump);
string.push_str("Hello");
string.push(' ');
string.push_str("world");
let s = string.into_bump_str();            // &str

// With capacity
let mut vec = Vec::with_capacity_in(100, &bump);
let mut string = String::with_capacity_in(100, &bump);
```

## SmallVec API Reference

### Core Types

```rust
use smallvec::{SmallVec, smallvec};

// Main type: SmallVec<[T; N]>
type SmallVec4<T> = SmallVec<[T; 4]>;      // Inline storage for 4 elements
type SmallVec8<T> = SmallVec<[T; 8]>;      // Inline storage for 8 elements

// Error type
pub enum CollectionAllocErr {
    CapacityOverflow,
    AllocErr { layout: Layout },
}
```

### Construction

```rust
use smallvec::{SmallVec, smallvec};

// Empty SmallVec
let mut vec: SmallVec<[i32; 4]> = SmallVec::new();

// Using macro
let vec = smallvec![1, 2, 3, 4];

// From slice
let slice = &[1, 2, 3];
let vec = SmallVec::<[i32; 4]>::from_slice(slice);

// From Vec
let std_vec = vec![1, 2, 3, 4, 5];
let small_vec = SmallVec::<[i32; 4]>::from_vec(std_vec);

// With capacity
let vec = SmallVec::<[i32; 4]>::with_capacity(10);

// From iterator
let vec: SmallVec<[i32; 4]> = (0..5).collect();
```

### Basic Operations

```rust
use smallvec::{SmallVec, smallvec};

let mut vec = smallvec![1, 2, 3];

// Push/pop
vec.push(4);
let last = vec.pop();                      // Some(4)

// Insert/remove
vec.insert(1, 10);                         // [1, 10, 2, 3]
let removed = vec.remove(1);               // 10, vec = [1, 2, 3]

// Indexing
vec[0] = 42;
let first = vec[0];                        // 42

// Length and capacity
let len = vec.len();                       // 3
let cap = vec.capacity();                  // 4 (inline) or more (heap)
let inline_size = vec.inline_size();       // 4

// Check if spilled to heap
let spilled = vec.spilled();               // false if ≤4 elements
```

### Capacity Management

```rust
use smallvec::{SmallVec, smallvec};

let mut vec = SmallVec::<[i32; 4]>::new();

// Reserve capacity
vec.reserve(10);                           // May spill to heap
vec.reserve_exact(10);                     // Exact capacity

// Shrink
vec.shrink_to_fit();                       // Minimize memory usage

// Try operations (fallible)
vec.try_reserve(10)?;                      // Result<(), CollectionAllocErr>
vec.try_reserve_exact(10)?;
```

### Conversion Operations

```rust
use smallvec::{SmallVec, smallvec};

let small = smallvec![1, 2, 3, 4, 5];

// To Vec
let std_vec: Vec<i32> = small.into_vec();

// To boxed slice
let small = smallvec![1, 2, 3];
let boxed: Box<[i32]> = small.into_boxed_slice();

// Into inner (if inline)
let small = smallvec![1, 2, 3];
if !small.spilled() {
    let array: [i32; 4] = small.into_inner().unwrap();
}
```

### Iteration and Slicing

```rust
use smallvec::{SmallVec, smallvec};

let vec = smallvec![1, 2, 3, 4, 5];

// Iteration
for item in &vec {
    println!("{}", item);
}

for item in vec.iter() {
    println!("{}", item);
}

// Mutable iteration
let mut vec = smallvec![1, 2, 3];
for item in &mut vec {
    *item *= 2;
}

// Slicing
let slice = &vec[1..3];                    // &[i32]
let slice = vec.as_slice();                // &[i32]
let slice = vec.as_mut_slice();            // &mut [i32]
```

### Advanced Operations

```rust
use smallvec::{SmallVec, smallvec};

let mut vec = smallvec![1, 2, 3, 4, 5];

// Extend
vec.extend([6, 7, 8]);
vec.extend_from_slice(&[9, 10]);

// Drain
let drained: SmallVec<[i32; 4]> = vec.drain(1..4).collect();

// Retain
vec.retain(|&x| x % 2 == 0);

// Dedup
vec.dedup();

// Split off
let mut vec = smallvec![1, 2, 3, 4, 5];
let tail = vec.split_off(2);               // vec=[1,2], tail=[3,4,5]

// Append
vec.append(&mut tail);                     // vec=[1,2,3,4,5], tail=[]
```

### Specialized Features

```rust
use smallvec::{SmallVec, smallvec};

// Extract if (requires "extract_if" feature)
#[cfg(feature = "extract_if")]
{
    let mut vec = smallvec![1, 2, 3, 4, 5, 6];
    let extracted: SmallVec<[i32; 4]> = vec.extract_if(|&x| x % 2 == 0).collect();
    // vec = [1, 3, 5], extracted = [2, 4, 6]
}

// Serde support (requires "serde" feature)
#[cfg(feature = "serde")]
{
    use serde::{Serialize, Deserialize};
    
    #[derive(Serialize, Deserialize)]
    struct Data {
        values: SmallVec<[i32; 4]>,
    }
}
```

## IndexMap API Reference

### Core Types

```rust
use indexmap::{IndexMap, IndexSet, indexmap, indexset};

// Main types
pub struct IndexMap<K, V, S = RandomState> { /* ... */ }
pub struct IndexSet<T, S = RandomState> { /* ... */ }

// Error types
pub struct TryReserveError { /* ... */ }
pub enum GetDisjointMutError {
    IndexOutOfBounds,
    OverlappingIndices,
}
```

### Construction

```rust
use indexmap::{IndexMap, indexmap};

// Empty map
let mut map = IndexMap::new();

// Using macro
let map = indexmap! {
    "key1" => "value1",
    "key2" => "value2",
    "key3" => "value3",
};

// With capacity
let map = IndexMap::with_capacity(100);

// With custom hasher
use fnv::FnvBuildHasher;
let map: IndexMap<String, i32, FnvBuildHasher> = IndexMap::default();

// From iterator
let data = [("a", 1), ("b", 2), ("c", 3)];
let map: IndexMap<&str, i32> = data.iter().cloned().collect();
```

### Basic Operations

```rust
use indexmap::{IndexMap, indexmap};

let mut map = IndexMap::new();

// Insert
map.insert("key1", 100);
map.insert("key2", 200);

// Get
let value = map.get("key1");               // Some(&100)
let value = map.get_mut("key1");           // Some(&mut 100)

// Contains
let exists = map.contains_key("key1");     // true

// Remove
let removed = map.remove("key1");          // Some(100)
let removed = map.remove_entry("key2");    // Some(("key2", 200))

// Length
let len = map.len();                       // 0 (after removals)
let is_empty = map.is_empty();             // true
```

### Index-Based Access

```rust
use indexmap::{IndexMap, indexmap};

let mut map = indexmap! {
    "first" => 1,
    "second" => 2,
    "third" => 3,
};

// Get by index
let (key, value) = map.get_index(0).unwrap();     // (&"first", &1)
let (key, value) = map.get_index_mut(1).unwrap(); // (&"second", &mut 2)

// Get index of key
let index = map.get_index_of("third");     // Some(2)

// Remove by index
let (key, value) = map.remove_index(0).unwrap(); // ("first", 1)

// Swap indices
map.swap_indices(0, 1);                    // Swap positions
```

### Iteration

```rust
use indexmap::{IndexMap, indexmap};

let map = indexmap! {
    "a" => 1,
    "b" => 2,
    "c" => 3,
};

// Iterate in insertion order
for (key, value) in &map {
    println!("{}: {}", key, value);
}

// Keys only
for key in map.keys() {
    println!("Key: {}", key);
}

// Values only
for value in map.values() {
    println!("Value: {}", value);
}

// Mutable values
let mut map = map;
for value in map.values_mut() {
    *value *= 2;
}

// Enumerate with index
for (index, (key, value)) in map.iter().enumerate() {
    println!("{}: {} => {}", index, key, value);
}
```

### Capacity Management

```rust
use indexmap::IndexMap;

let mut map = IndexMap::new();

// Reserve capacity
map.reserve(100);                          // May reallocate
map.try_reserve(100)?;                     // Fallible version

// Shrink
map.shrink_to_fit();                       // Minimize memory usage
map.shrink_to(50);                         // Shrink to at least 50

// Capacity info
let capacity = map.capacity();             // Current capacity
```

### Sorting and Reordering

```rust
use indexmap::{IndexMap, indexmap};

let mut map = indexmap! {
    "zebra" => 26,
    "apple" => 1,
    "banana" => 2,
};

// Sort by keys
map.sort_keys();                           // apple, banana, zebra

// Sort by values
map.sort_by(|_k1, v1, _k2, v2| v1.cmp(v2)); // apple(1), banana(2), zebra(26)

// Sort with custom comparator
map.sort_by(|k1, _v1, k2, _v2| k1.len().cmp(&k2.len()));

// Sort unstable (potentially faster)
map.sort_unstable_keys();
map.sort_unstable_by(|_k1, v1, _k2, v2| v1.cmp(v2));

// Reverse order
map.reverse();
```

### Entry API

```rust
use indexmap::{IndexMap, map::Entry};

let mut map = IndexMap::new();

// Entry API
match map.entry("key") {
    Entry::Occupied(mut entry) => {
        *entry.get_mut() += 1;
    }
    Entry::Vacant(entry) => {
        entry.insert(1);
    }
}

// Or more concisely
*map.entry("key").or_insert(0) += 1;

// With index information
let entry = map.entry("new_key");
let index = entry.index();                 // Index where it will be inserted
entry.or_insert(42);
```

### Slicing and Ranges

```rust
use indexmap::{IndexMap, indexmap};

let map = indexmap! {
    "a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5
};

// Get slice of entries
let slice = map.get_range(1..4).unwrap();  // b, c, d
for (key, value) in slice {
    println!("{}: {}", key, value);
}

// Mutable slice
let mut map = map;
let slice_mut = map.get_range_mut(1..3).unwrap();
for (_key, value) in slice_mut {
    *value *= 10;
}

// Multiple disjoint ranges
let indices = [0, 2, 4];
let slices = map.get_disjoint_indices_mut(&indices).unwrap();
for slice in slices {
    *slice.1 += 100;
}
```

### Advanced Operations

```rust
use indexmap::{IndexMap, indexmap};

let mut map = indexmap! {
    "a" => 1,
    "b" => 2,
    "c" => 3,
};

// Extend
map.extend([("d", 4), ("e", 5)]);

// Drain
let drained: IndexMap<_, _> = map.drain(1..3).collect(); // b, c

// Retain
map.retain(|k, v| k != "a" && *v > 1);

// Clear
map.clear();                               // Remove all entries

// Clone
let map2 = map.clone();                    // Deep copy
```

### Equivalent Keys

```rust
use indexmap::{IndexMap, Equivalent};

// Custom equivalent implementation
#[derive(Hash, PartialEq, Eq)]
struct CaseInsensitive(String);

impl Equivalent<CaseInsensitive> for str {
    fn equivalent(&self, key: &CaseInsensitive) -> bool {
        self.to_lowercase() == key.0.to_lowercase()
    }
}

let mut map = IndexMap::new();
map.insert(CaseInsensitive("Hello".to_string()), 1);

// Can lookup with &str using Equivalent
assert_eq!(map.get("hello"), Some(&1));
assert_eq!(map.get("HELLO"), Some(&1));
```

### IndexSet Operations

```rust
use indexmap::{IndexSet, indexset};

let mut set = IndexSet::new();

// Insert
set.insert("apple");
set.insert("banana");
set.insert("cherry");

// Contains
let exists = set.contains("apple");        // true

// Remove
let removed = set.remove("banana");        // true

// Index operations
let item = set.get_index(0);               // Some(&"apple")
let index = set.get_index_of("cherry");    // Some(2)

// Set operations
let set1 = indexset!{"a", "b", "c"};
let set2 = indexset!{"b", "c", "d"};

let union: IndexSet<_> = set1.union(&set2).cloned().collect();
let intersection: IndexSet<_> = set1.intersection(&set2).cloned().collect();
let difference: IndexSet<_> = set1.difference(&set2).cloned().collect();
```

## Integration Examples

### Compiler Symbol Table

```rust
use bumpalo::Bump;
use smallvec::{SmallVec, smallvec};
use indexmap::IndexMap;

pub struct CompilerContext<'arena> {
    // Arena for temporary allocations
    arena: &'arena Bump,
    
    // Symbol table with insertion order
    symbols: IndexMap<String, Symbol<'arena>>,
    
    // Scope stack (usually shallow)
    scope_stack: SmallVec<[ScopeId; 8]>,
    
    // Error collection
    errors: SmallVec<[CompileError; 16]>,
}

#[derive(Debug)]
pub struct Symbol<'arena> {
    pub name: &'arena str,
    pub symbol_type: SymbolType,
    pub location: SourceLocation,
    pub scope: ScopeId,
}

impl<'arena> CompilerContext<'arena> {
    pub fn new(arena: &'arena Bump) -> Self {
        Self {
            arena,
            symbols: IndexMap::new(),
            scope_stack: smallvec![ScopeId::global()],
            errors: SmallVec::new(),
        }
    }
    
    pub fn declare_symbol(&mut self, name: &str, symbol_type: SymbolType, location: SourceLocation) -> Result<(), CompileError> {
        // Intern the name in arena
        let interned_name = self.arena.alloc_str(name);
        
        // Check for redeclaration in current scope
        let current_scope = *self.scope_stack.last().unwrap();
        if self.symbols.values().any(|s| s.name == name && s.scope == current_scope) {
            return Err(CompileError::SymbolRedeclaration {
                name: name.to_string(),
                location,
            });
        }
        
        // Create symbol
        let symbol = Symbol {
            name: interned_name,
            symbol_type,
            location,
            scope: current_scope,
        };
        
        self.symbols.insert(name.to_string(), symbol);
        Ok(())
    }
    
    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol<'arena>> {
        // Search from innermost to outermost scope
        for &scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = self.symbols.values().find(|s| s.name == name && s.scope == scope) {
                return Some(symbol);
            }
        }
        None
    }
    
    pub fn enter_scope(&mut self) -> ScopeId {
        let new_scope = ScopeId::new();
        self.scope_stack.push(new_scope);
        new_scope
    }
    
    pub fn exit_scope(&mut self) -> Option<ScopeId> {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop()
        } else {
            None // Can't exit global scope
        }
    }
    
    pub fn symbols_in_declaration_order(&self) -> impl Iterator<Item = (&String, &Symbol<'arena>)> {
        self.symbols.iter()
    }
}
```

### AST Builder with Arena Allocation

```rust
use bumpalo::Bump;
use smallvec::{SmallVec, smallvec};

pub struct AstBuilder<'arena> {
    arena: &'arena Bump,
}

#[derive(Debug)]
pub struct AstNode<'arena> {
    pub kind: NodeKind<'arena>,
    pub span: SourceSpan,
}

#[derive(Debug)]
pub enum NodeKind<'arena> {
    Literal(LiteralValue),
    Identifier(&'arena str),
    BinaryOp {
        op: BinaryOperator,
        left: &'arena AstNode<'arena>,
        right: &'arena AstNode<'arena>,
    },
    FunctionCall {
        function: &'arena AstNode<'arena>,
        args: &'arena [&'arena AstNode<'arena>],
    },
    Block {
        statements: &'arena [&'arena AstNode<'arena>],
    },
}

impl<'arena> AstBuilder<'arena> {
    pub fn new(arena: &'arena Bump) -> Self {
        Self { arena }
    }
    
    pub fn literal(&self, value: LiteralValue, span: SourceSpan) -> &'arena AstNode<'arena> {
        self.arena.alloc(AstNode {
            kind: NodeKind::Literal(value),
            span,
        })
    }
    
    pub fn identifier(&self, name: &str, span: SourceSpan) -> &'arena AstNode<'arena> {
        let interned_name = self.arena.alloc_str(name);
        self.arena.alloc(AstNode {
            kind: NodeKind::Identifier(interned_name),
            span,
        })
    }
    
    pub fn binary_op(
        &self,
        op: BinaryOperator,
        left: &'arena AstNode<'arena>,
        right: &'arena AstNode<'arena>,
        span: SourceSpan,
    ) -> &'arena AstNode<'arena> {
        self.arena.alloc(AstNode {
            kind: NodeKind::BinaryOp { op, left, right },
            span,
        })
    }
    
    pub fn function_call(
        &self,
        function: &'arena AstNode<'arena>,
        args: SmallVec<[&'arena AstNode<'arena>; 4]>,
        span: SourceSpan,
    ) -> &'arena AstNode<'arena> {
        let args_slice = self.arena.alloc_slice_fill_iter(args.into_iter());
        self.arena.alloc(AstNode {
            kind: NodeKind::FunctionCall {
                function,
                args: args_slice,
            },
            span,
        })
    }
    
    pub fn block(
        &self,
        statements: SmallVec<[&'arena AstNode<'arena>; 8]>,
        span: SourceSpan,
    ) -> &'arena AstNode<'arena> {
        let statements_slice = self.arena.alloc_slice_fill_iter(statements.into_iter());
        self.arena.alloc(AstNode {
            kind: NodeKind::Block {
                statements: statements_slice,
            },
            span,
        })
    }
}

// Usage example
fn parse_expression(arena: &Bump, tokens: &[Token]) -> Result<&AstNode, ParseError> {
    let builder = AstBuilder::new(arena);
    
    // Parse: 2 + 3 * 4
    let two = builder.literal(LiteralValue::Number(2.0), SourceSpan::new(0, 1));
    let three = builder.literal(LiteralValue::Number(3.0), SourceSpan::new(4, 5));
    let four = builder.literal(LiteralValue::Number(4.0), SourceSpan::new(8, 9));
    
    let multiply = builder.binary_op(
        BinaryOperator::Multiply,
        three,
        four,
        SourceSpan::new(4, 9),
    );
    
    let add = builder.binary_op(
        BinaryOperator::Add,
        two,
        multiply,
        SourceSpan::new(0, 9),
    );
    
    Ok(add)
}
```

### Configuration Management

```rust
use indexmap::IndexMap;
use smallvec::{SmallVec, smallvec};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct CompilerConfig {
    // Preserve order for config file output
    #[serde(with = "indexmap::serde_seq")]
    pub settings: IndexMap<String, ConfigValue>,
    
    // Small collections for common options
    pub optimization_passes: SmallVec<[String; 8]>,
    pub include_paths: SmallVec<[String; 4]>,
    pub feature_flags: SmallVec<[String; 16]>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum ConfigValue {
    String(String),
    Number(f64),
    Boolean(bool),
    Array(Vec<ConfigValue>),
    Object(IndexMap<String, ConfigValue>),
}

impl CompilerConfig {
    pub fn new() -> Self {
        let mut settings = IndexMap::new();
        settings.insert("debug".to_string(), ConfigValue::Boolean(false));
        settings.insert("optimize".to_string(), ConfigValue::Boolean(true));
        settings.insert("target".to_string(), ConfigValue::String("x86_64".to_string()));
        
        Self {
            settings,
            optimization_passes: smallvec![
                "inline".to_string(),
                "dce".to_string(),
                "constant_folding".to_string(),
            ],
            include_paths: smallvec![
                "/usr/include".to_string(),
                "/usr/local/include".to_string(),
            ],
            feature_flags: SmallVec::new(),
        }
    }
    
    pub fn load_from_file(path: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(path)?;
        let config: Self = toml::from_str(&content)?;
        Ok(config)
    }
    
    pub fn save_to_file(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let content = toml::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }
    
    pub fn merge(&mut self, other: Self) {
        // Merge settings (IndexMap preserves order)
        for (key, value) in other.settings {
            self.settings.insert(key, value);
        }
        
        // Extend arrays
        self.optimization_passes.extend(other.optimization_passes);
        self.include_paths.extend(other.include_paths);
        self.feature_flags.extend(other.feature_flags);
    }
    
    pub fn get_string(&self, key: &str) -> Option<&str> {
        match self.settings.get(key)? {
            ConfigValue::String(s) => Some(s),
            _ => None,
        }
    }
    
    pub fn get_bool(&self, key: &str) -> Option<bool> {
        match self.settings.get(key)? {
            ConfigValue::Boolean(b) => Some(*b),
            _ => None,
        }
    }
    
    pub fn get_number(&self, key: &str) -> Option<f64> {
        match self.settings.get(key)? {
            ConfigValue::Number(n) => Some(*n),
            _ => None,
        }
    }
}
```

## Performance Comparison

### Allocation Performance

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use bumpalo::Bump;
use smallvec::{SmallVec, smallvec};
use indexmap::IndexMap;
use std::collections::HashMap;

fn bench_allocations(c: &mut Criterion) {
    let mut group = c.benchmark_group("allocations");
    
    for size in [10, 100, 1000].iter() {
        // Vec vs SmallVec
        group.bench_with_input(BenchmarkId::new("vec", size), size, |b, &size| {
            b.iter(|| {
                let mut v = Vec::new();
                for i in 0..size {
                    v.push(black_box(i));
                }
                v
            });
        });
        
        group.bench_with_input(BenchmarkId::new("smallvec", size), size, |b, &size| {
            b.iter(|| {
                let mut v = SmallVec::<[i32; 8]>::new();
                for i in 0..size {
                    v.push(black_box(i));
                }
                v
            });
        });
        
        // HashMap vs IndexMap
        group.bench_with_input(BenchmarkId::new("hashmap", size), size, |b, &size| {
            b.iter(|| {
                let mut m = HashMap::new();
                for i in 0..size {
                    m.insert(black_box(i), black_box(i * 2));
                }
                m
            });
        });
        
        group.bench_with_input(BenchmarkId::new("indexmap", size), size, |b, &size| {
            b.iter(|| {
                let mut m = IndexMap::new();
                for i in 0..size {
                    m.insert(black_box(i), black_box(i * 2));
                }
                m
            });
        });
        
        // Heap vs Arena allocation
        group.bench_with_input(BenchmarkId::new("heap", size), size, |b, &size| {
            b.iter(|| {
                let mut v = Vec::new();
                for i in 0..size {
                    v.push(Box::new(black_box(i)));
                }
                v
            });
        });
        
        group.bench_with_input(BenchmarkId::new("arena", size), size, |b, &size| {
            b.iter(|| {
                let bump = Bump::new();
                let mut v = Vec::new();
                for i in 0..size {
                    v.push(bump.alloc(black_box(i)));
                }
                v
            });
        });
    }
    
    group.finish();
}

criterion_group!(benches, bench_allocations);
criterion_main!(benches);
```

### Memory Usage Comparison

```rust
use std::mem;

fn analyze_memory_usage() {
    println!("Memory usage analysis:");
    
    // Empty containers
    println!("Empty containers:");
    println!("  Vec<i32>: {} bytes", mem::size_of::<Vec<i32>>());
    println!("  SmallVec<[i32; 4]>: {} bytes", mem::size_of::<SmallVec<[i32; 4]>>());
    println!("  SmallVec<[i32; 8]>: {} bytes", mem::size_of::<SmallVec<[i32; 8]>>());
    println!("  HashMap<i32, i32>: {} bytes", mem::size_of::<std::collections::HashMap<i32, i32>>());
    println!("  IndexMap<i32, i32>: {} bytes", mem::size_of::<IndexMap<i32, i32>>());
    
    // With data
    let vec = vec![1, 2, 3, 4, 5];
    let small_vec = smallvec![1, 2, 3, 4, 5];
    let mut hash_map = std::collections::HashMap::new();
    let mut index_map = IndexMap::new();
    
    for i in 1..=5 {
        hash_map.insert(i, i * 2);
        index_map.insert(i, i * 2);
    }
    
    println!("\nWith 5 elements:");
    println!("  Vec: {} bytes stack + {} bytes heap", 
        mem::size_of_val(&vec), 
        vec.capacity() * mem::size_of::<i32>());
    
    println!("  SmallVec: {} bytes stack + {} bytes heap", 
        mem::size_of_val(&small_vec),
        if small_vec.spilled() { small_vec.capacity() * mem::size_of::<i32>() } else { 0 });
    
    println!("  HashMap: {} bytes stack + ~{} bytes heap", 
        mem::size_of_val(&hash_map),
        hash_map.capacity() * (mem::size_of::<i32>() * 2 + 8)); // Rough estimate
    
    println!("  IndexMap: {} bytes stack + ~{} bytes heap", 
        mem::size_of_val(&index_map),
        index_map.capacity() * (mem::size_of::<i32>() * 2 + 8)); // Rough estimate
}
```

## Common Patterns

### Builder Pattern with Arena

```rust
use bumpalo::Bump;
use smallvec::{SmallVec, smallvec};

pub struct QueryBuilder<'arena> {
    arena: &'arena Bump,
    select_fields: SmallVec<[&'arena str; 8]>,
    from_table: Option<&'arena str>,
    where_conditions: SmallVec<[&'arena str; 4]>,
    order_by: SmallVec<[&'arena str; 2]>,
}

impl<'arena> QueryBuilder<'arena> {
    pub fn new(arena: &'arena Bump) -> Self {
        Self {
            arena,
            select_fields: SmallVec::new(),
            from_table: None,
            where_conditions: SmallVec::new(),
            order_by: SmallVec::new(),
        }
    }
    
    pub fn select(mut self, field: &str) -> Self {
        let interned_field = self.arena.alloc_str(field);
        self.select_fields.push(interned_field);
        self
    }
    
    pub fn from(mut self, table: &str) -> Self {
        self.from_table = Some(self.arena.alloc_str(table));
        self
    }
    
    pub fn where_clause(mut self, condition: &str) -> Self {
        let interned_condition = self.arena.alloc_str(condition);
        self.where_conditions.push(interned_condition);
        self
    }
    
    pub fn order_by(mut self, field: &str) -> Self {
        let interned_field = self.arena.alloc_str(field);
        self.order_by.push(interned_field);
        self
    }
    
    pub fn build(self) -> &'arena str {
        let mut query = bumpalo::collections::String::new_in(self.arena);
        
        // SELECT clause
        query.push_str("SELECT ");
        if self.select_fields.is_empty() {
            query.push('*');
        } else {
            for (i, field) in self.select_fields.iter().enumerate() {
                if i > 0 { query.push_str(", "); }
                query.push_str(field);
            }
        }
        
        // FROM clause
        if let Some(table) = self.from_table {
            query.push_str(" FROM ");
            query.push_str(table);
        }
        
        // WHERE clause
        if !self.where_conditions.is_empty() {
            query.push_str(" WHERE ");
            for (i, condition) in self.where_conditions.iter().enumerate() {
                if i > 0 { query.push_str(" AND "); }
                query.push_str(condition);
            }
        }
        
        // ORDER BY clause
        if !self.order_by.is_empty() {
            query.push_str(" ORDER BY ");
            for (i, field) in self.order_by.iter().enumerate() {
                if i > 0 { query.push_str(", "); }
                query.push_str(field);
            }
        }
        
        query.into_bump_str()
    }
}

// Usage
fn build_query(arena: &Bump) -> &str {
    QueryBuilder::new(arena)
        .select("name")
        .select("email")
        .from("users")
        .where_clause("active = true")
        .where_clause("age > 18")
        .order_by("name")
        .build()
}
```

### Cache with Ordered Eviction

```rust
use indexmap::IndexMap;

pub struct LruCache<K, V> {
    map: IndexMap<K, V>,
    capacity: usize,
}

impl<K, V> LruCache<K, V> 
where 
    K: std::hash::Hash + Eq + Clone,
{
    pub fn new(capacity: usize) -> Self {
        Self {
            map: IndexMap::with_capacity(capacity),
            capacity,
        }
    }
    
    pub fn get(&mut self, key: &K) -> Option<&V> {
        // Move to end (most recently used)
        if let Some(value) = self.map.remove(key) {
            self.map.insert(key.clone(), value);
            self.map.get(key)
        } else {
            None
        }
    }
    
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        // Move to end (most recently used)
        if let Some(value) = self.map.remove(key) {
            self.map.insert(key.clone(), value);
            self.map.get_mut(key)
        } else {
            None
        }
    }
    
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(old_value) = self.map.remove(&key) {
            // Update existing key
            self.map.insert(key, value);
            Some(old_value)
        } else {
            // New key
            if self.map.len() >= self.capacity {
                // Remove least recently used (first item)
                self.map.remove_index(0);
            }
            self.map.insert(key, value);
            None
        }
    }
    
    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.map.remove(key)
    }
    
    pub fn len(&self) -> usize {
        self.map.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
    
    pub fn capacity(&self) -> usize {
        self.capacity
    }
    
    pub fn clear(&mut self) {
        self.map.clear();
    }
    
    // Iterate in access order (most recent last)
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.map.iter()
    }
}

// Usage example
fn cache_example() {
    let mut cache = LruCache::new(3);
    
    cache.insert("a", 1);
    cache.insert("b", 2);
    cache.insert("c", 3);
    
    // Access "a" to make it most recent
    cache.get(&"a");
    
    // Insert "d" - should evict "b" (least recent)
    cache.insert("d", 4);
    
    assert!(cache.get(&"b").is_none()); // "b" was evicted
    assert!(cache.get(&"a").is_some()); // "a" is still there
    assert!(cache.get(&"c").is_some()); // "c" is still there
    assert!(cache.get(&"d").is_some()); // "d" was just inserted
}
```

## Error Handling

### Comprehensive Error Types

```rust
use bumpalo::AllocErr;
use smallvec::CollectionAllocErr;
use indexmap::TryReserveError;

#[derive(Debug, thiserror::Error)]
pub enum CompilerError {
    #[error("Memory allocation failed")]
    AllocationError(#[from] AllocErr),
    
    #[error("Collection allocation failed: {0}")]
    CollectionError(#[from] CollectionAllocErr),
    
    #[error("Reserve failed: {0}")]
    ReserveError(#[from] TryReserveError),
    
    #[error("Parse error at {location}: {message}")]
    ParseError {
        message: String,
        location: SourceLocation,
    },
    
    #[error("Type error: {message}")]
    TypeError {
        message: String,
        expected: String,
        found: String,
    },
    
    #[error("Symbol '{name}' not found")]
    SymbolNotFound {
        name: String,
    },
    
    #[error("Symbol '{name}' already declared at {previous_location}")]
    SymbolRedeclaration {
        name: String,
        previous_location: SourceLocation,
    },
}

// Error recovery with arena allocation
pub struct ErrorRecovery<'arena> {
    arena: &'arena Bump,
    errors: SmallVec<[CompilerError; 16]>,
    recovery_points: SmallVec<[RecoveryPoint; 8]>,
}

#[derive(Debug, Clone)]
pub struct RecoveryPoint {
    pub position: usize,
    pub context: RecoveryContext,
}

#[derive(Debug, Clone)]
pub enum RecoveryContext {
    Statement,
    Expression,
    Declaration,
    Block,
}

impl<'arena> ErrorRecovery<'arena> {
    pub fn new(arena: &'arena Bump) -> Self {
        Self {
            arena,
            errors: SmallVec::new(),
            recovery_points: SmallVec::new(),
        }
    }
    
    pub fn report_error(&mut self, error: CompilerError) {
        self.errors.push(error);
    }
    
    pub fn add_recovery_point(&mut self, position: usize, context: RecoveryContext) {
        self.recovery_points.push(RecoveryPoint { position, context });
    }
    
    pub fn try_recover(&mut self, current_position: usize) -> Option<usize> {
        // Find the nearest recovery point
        self.recovery_points
            .iter()
            .filter(|rp| rp.position > current_position)
            .min_by_key(|rp| rp.position)
            .map(|rp| rp.position)
    }
    
    pub fn format_errors(&self) -> &'arena str {
        let mut output = bumpalo::collections::String::new_in(self.arena);
        
        for (i, error) in self.errors.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }
            output.push_str(&format!("Error {}: {}", i + 1, error));
        }
        
        output.into_bump_str()
    }
    
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }
}
```

## Advanced Usage

### Custom Allocators and Memory Management

```rust
use bumpalo::Bump;
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

// Custom allocator that tracks allocations
pub struct TrackingAllocator {
    inner: System,
    allocated: AtomicUsize,
    allocation_count: AtomicUsize,
}

impl TrackingAllocator {
    pub const fn new() -> Self {
        Self {
            inner: System,
            allocated: AtomicUsize::new(0),
            allocation_count: AtomicUsize::new(0),
        }
    }
    
    pub fn allocated_bytes(&self) -> usize {
        self.allocated.load(Ordering::Relaxed)
    }
    
    pub fn allocation_count(&self) -> usize {
        self.allocation_count.load(Ordering::Relaxed)
    }
    
    pub fn reset(&self) {
        self.allocated.store(0, Ordering::Relaxed);
        self.allocation_count.store(0, Ordering::Relaxed);
    }
}

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = self.inner.alloc(layout);
        if !ptr.is_null() {
            self.allocated.fetch_add(layout.size(), Ordering::Relaxed);
            self.allocation_count.fetch_add(1, Ordering::Relaxed);
        }
        ptr
    }
    
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.inner.dealloc(ptr, layout);
        self.allocated.fetch_sub(layout.size(), Ordering::Relaxed);
    }
}

// Memory-aware compilation
pub struct MemoryAwareCompiler {
    allocator: &'static TrackingAllocator,
    memory_limit: usize,
    arena_pool: Vec<Bump>,
}

impl MemoryAwareCompiler {
    pub fn new(allocator: &'static TrackingAllocator, memory_limit: usize) -> Self {
        Self {
            allocator,
            memory_limit,
            arena_pool: Vec::new(),
        }
    }
    
    pub fn compile_with_memory_limit(&mut self, source: &str) -> Result<CompiledModule, CompilerError> {
        // Reset allocation tracking
        self.allocator.reset();
        
        // Get arena from pool or create new one
        let arena = self.get_arena();
        
        // Compile with memory monitoring
        let result = self.compile_in_arena(&arena, source);
        
        // Check memory usage
        let allocated = self.allocator.allocated_bytes();
        if allocated > self.memory_limit {
            return Err(CompilerError::MemoryLimitExceeded {
                limit: self.memory_limit,
                used: allocated,
            });
        }
        
        // Return arena to pool
        self.return_arena(arena);
        
        result
    }
    
    fn get_arena(&mut self) -> Bump {
        self.arena_pool.pop().unwrap_or_else(|| Bump::new())
    }
    
    fn return_arena(&mut self, mut arena: Bump) {
        arena.reset();
        if self.arena_pool.len() < 10 { // Limit pool size
            self.arena_pool.push(arena);
        }
    }
    
    fn compile_in_arena(&self, arena: &Bump, source: &str) -> Result<CompiledModule, CompilerError> {
        // Implementation would use the arena for temporary allocations
        todo!("Implement compilation logic")
    }
}
```

### Thread-Safe Collections

```rust
use indexmap::IndexMap;
use smallvec::{SmallVec, smallvec};
use std::sync::{Arc, RwLock, Mutex};

// Thread-safe symbol table
pub struct ThreadSafeSymbolTable {
    symbols: Arc<RwLock<IndexMap<String, Symbol>>>,
    scopes: Arc<Mutex<SmallVec<[ScopeId; 8]>>>,
}

impl ThreadSafeSymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: Arc::new(RwLock::new(IndexMap::new())),
            scopes: Arc::new(Mutex::new(smallvec![ScopeId::global()])),
        }
    }
    
    pub fn declare_symbol(&self, name: String, symbol: Symbol) -> Result<(), String> {
        let mut symbols = self.symbols.write().unwrap();
        
        if symbols.contains_key(&name) {
            return Err(format!("Symbol '{}' already declared", name));
        }
        
        symbols.insert(name, symbol);
        Ok(())
    }
    
    pub fn lookup_symbol(&self, name: &str) -> Option<Symbol> {
        let symbols = self.symbols.read().unwrap();
        symbols.get(name).cloned()
    }
    
    pub fn enter_scope(&self) -> ScopeId {
        let mut scopes = self.scopes.lock().unwrap();
        let new_scope = ScopeId::new();
        scopes.push(new_scope);
        new_scope
    }
    
    pub fn exit_scope(&self) -> Option<ScopeId> {
        let mut scopes = self.scopes.lock().unwrap();
        if scopes.len() > 1 {
            scopes.pop()
        } else {
            None
        }
    }
    
    pub fn symbols_in_declaration_order(&self) -> Vec<(String, Symbol)> {
        let symbols = self.symbols.read().unwrap();
        symbols.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }
}

impl Clone for ThreadSafeSymbolTable {
    fn clone(&self) -> Self {
        Self {
            symbols: Arc::clone(&self.symbols),
            scopes: Arc::clone(&self.scopes),
        }
    }
}

// Parallel compilation with shared state
pub fn parallel_compile(modules: Vec<Module>) -> Result<Vec<CompiledModule>, CompilerError> {
    use rayon::prelude::*;
    
    let symbol_table = ThreadSafeSymbolTable::new();
    
    // First pass: collect all symbols
    for module in &modules {
        let symbols = extract_symbols(&module.source)?;
        for (name, symbol) in symbols {
            symbol_table.declare_symbol(name, symbol)?;
        }
    }
    
    // Second pass: compile in parallel
    let results: Result<Vec<_>, _> = modules
        .par_iter()
        .map(|module| {
            compile_module_with_symbols(module, &symbol_table)
        })
        .collect();
    
    results
}

fn extract_symbols(source: &str) -> Result<Vec<(String, Symbol)>, CompilerError> {
    // Implementation would parse and extract symbol declarations
    todo!("Implement symbol extraction")
}

fn compile_module_with_symbols(
    module: &Module, 
    symbol_table: &ThreadSafeSymbolTable
) -> Result<CompiledModule, CompilerError> {
    // Implementation would compile the module using the shared symbol table
    todo!("Implement module compilation")
}
```

---

This comprehensive API reference provides detailed examples and practical usage patterns for bumpalo, smallvec, and indexmap. The examples demonstrate real-world integration scenarios particularly relevant for compiler implementations like Parallax, showing how these libraries can be effectively combined to create high-performance, memory-efficient systems.