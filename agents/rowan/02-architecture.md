# Green and Red Tree Architecture

Rowan's core innovation is its dual-tree architecture that separates immutable structure from mutable views. This design enables both memory efficiency and API convenience.

## Overview

Rowan uses two complementary tree representations:

- **Green Tree**: Immutable, structural sharing, no parent pointers, relative offsets
- **Red Tree**: Mutable view, parent pointers, absolute positions, convenient API

This separation allows Rowan to achieve:
- **Memory efficiency** through structural sharing
- **Immutability** for safe concurrent access
- **Convenience** through rich navigation APIs
- **Incremental parsing** through tree reuse

## Green Tree (Immutable Structure)

The green tree is the fundamental immutable representation that stores the actual syntax tree structure.

### Key Characteristics

```rust
// Green nodes are immutable and shareable
pub struct GreenNode {
    ptr: ThinArc<GreenNodeHead, GreenChild>,
}

pub struct GreenToken {
    ptr: ThinArc<GreenTokenHead, u8>,
}
```

**Properties:**
- **Immutable**: Once created, never changes
- **Structural sharing**: Common subtrees are shared between different trees
- **No parent pointers**: Nodes don't know their parents
- **Relative offsets**: Child positions are relative to parent
- **Compact**: Optimized memory layout using thin pointers

### Green Tree Structure

```rust
// Internal representation of green nodes
struct GreenNodeHead {
    kind: SyntaxKind,
    text_len: TextSize,
    _c: Count<GreenNode>,  // For memory tracking
}

enum GreenChild {
    Node { rel_offset: TextSize, node: GreenNode },
    Token { rel_offset: TextSize, token: GreenToken },
}
```

### Memory Layout

Green trees use several optimizations:

1. **Thin Arc Pointers**: Reduce memory overhead
2. **Inline Small Tokens**: Store short text directly in the token
3. **Structural Sharing**: Identical subtrees share the same memory
4. **Compact Children**: Children stored as a contiguous array

```rust
// Example of structural sharing
let green1 = build_tree("fn foo() {}");
let green2 = build_tree("fn bar() {}");
// The function body "{}" subtree is shared between both trees
```

### Building Green Trees

Green trees are built using `GreenNodeBuilder`:

```rust
use rowan::GreenNodeBuilder;

let mut builder = GreenNodeBuilder::new();

// Start a node
builder.start_node(SyntaxKind::FUNCTION.into());

// Add tokens and child nodes
builder.token(SyntaxKind::FN_KW.into(), "fn");
builder.token(SyntaxKind::WHITESPACE.into(), " ");
builder.token(SyntaxKind::IDENT.into(), "foo");

// Nested nodes
builder.start_node(SyntaxKind::PARAM_LIST.into());
builder.token(SyntaxKind::L_PAREN.into(), "(");
builder.token(SyntaxKind::R_PAREN.into(), ")");
builder.finish_node();

builder.finish_node();

let green = builder.finish();
```

## Red Tree (Mutable View)

The red tree provides a convenient API over the green tree with parent pointers and absolute positions.

### Key Characteristics

```rust
pub struct SyntaxNode<L: Language> {
    raw: cursor::SyntaxNode,
    _p: PhantomData<L>,
}

pub struct SyntaxToken<L: Language> {
    raw: cursor::SyntaxToken,
    _p: PhantomData<L>,
}
```

**Properties:**
- **Parent pointers**: Nodes know their parents
- **Absolute positions**: Text ranges are absolute
- **Rich API**: Convenient navigation and query methods
- **Lazy computation**: Parent pointers computed on demand
- **Type safe**: Parameterized by language type

### Red Tree Navigation

```rust
// Create a red tree from a green tree
let syntax: SyntaxNode<MyLang> = SyntaxNode::new_root(green);

// Navigate the tree
let parent = syntax.parent();
let children: Vec<SyntaxNode<MyLang>> = syntax.children().collect();
let first_child = syntax.first_child();
let last_child = syntax.last_child();

// Sibling navigation
let next_sibling = syntax.next_sibling();
let prev_sibling = syntax.prev_sibling();

// Token access
let first_token = syntax.first_token();
let last_token = syntax.last_token();

// Text and ranges
let text = syntax.text();
let range = syntax.text_range();
```

### Cursor-Based Implementation

Internally, red trees use a cursor-based approach for efficient navigation:

```rust
// Internal cursor structure
pub(crate) struct SyntaxNode {
    green: GreenNode,
    parent: Option<(SyntaxNode, usize)>,
    index: usize,
    offset: TextSize,
}
```

The cursor tracks:
- **Green node**: Reference to the immutable structure
- **Parent**: Parent node and child index
- **Index**: Position among siblings
- **Offset**: Absolute text position

## Relationship Between Green and Red

```
Green Tree (Immutable)          Red Tree (Mutable View)
┌─────────────────┐            ┌─────────────────┐
│   GreenNode     │◄───────────│   SyntaxNode    │
│                 │            │                 │
│ - kind          │            │ - parent ptr    │
│ - text_len      │            │ - absolute pos  │
│ - children[]    │            │ - rich API      │
└─────────────────┘            └─────────────────┘
        │                              │
        │ structural sharing           │ navigation
        ▼                              ▼
┌─────────────────┐            ┌─────────────────┐
│   GreenChild    │            │  SyntaxElement  │
│                 │            │                 │
│ - rel_offset    │            │ - absolute pos  │
│ - node/token    │            │ - parent access │
└─────────────────┘            └─────────────────┘
```

## Memory Efficiency Benefits

### Structural Sharing

```rust
// Example: Two similar functions share common structure
fn parse_functions() {
    let func1 = parse("fn foo() { println!(\"hello\"); }");
    let func2 = parse("fn bar() { println!(\"hello\"); }");
    
    // The println! expression subtree is shared between both functions
    // Only the function names and signatures are different
}
```

### Copy-on-Write Semantics

```rust
// Modifying a tree creates a new tree with minimal copying
let original_tree = parse("fn foo() {}");
let modified_tree = original_tree.replace_child(
    old_child,
    new_child
);
// Only the path from root to the changed node is copied
// All other subtrees are shared
```

### Memory Usage Comparison

```
Traditional AST:
- Each node stores all data
- No sharing between similar structures
- Parent pointers in every node
- Higher memory overhead

Rowan Green Tree:
- Structural sharing of identical subtrees
- No parent pointers in green nodes
- Compact representation
- Significantly lower memory usage for large codebases
```

## Performance Characteristics

### Green Tree Operations

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Creation | O(n) | Linear in tree size |
| Cloning | O(1) | Reference counting |
| Equality | O(1) | Pointer comparison for shared nodes |
| Memory | O(unique nodes) | Shared subtrees counted once |

### Red Tree Operations

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Navigation | O(1) | Direct pointer access |
| Parent access | O(1) | Cached parent pointers |
| Text range | O(1) | Precomputed absolute positions |
| Tree traversal | O(n) | Linear in visited nodes |

## Practical Examples

### Building and Using Trees

```rust
use rowan::{GreenNodeBuilder, SyntaxNode, Language};

// Build a green tree
fn build_expression() -> GreenNode {
    let mut builder = GreenNodeBuilder::new();
    
    builder.start_node(SyntaxKind::BINARY_EXPR.into());
    
    // Left operand
    builder.token(SyntaxKind::NUMBER.into(), "42");
    
    // Operator
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.token(SyntaxKind::PLUS.into(), "+");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // Right operand
    builder.token(SyntaxKind::NUMBER.into(), "13");
    
    builder.finish_node();
    builder.finish()
}

// Use the red tree API
fn analyze_expression(green: GreenNode) {
    let syntax: SyntaxNode<MyLang> = SyntaxNode::new_root(green);
    
    println!("Expression: {}", syntax.text());
    println!("Range: {:?}", syntax.text_range());
    
    // Find all number tokens
    for token in syntax.descendants_with_tokens() {
        if let rowan::NodeOrToken::Token(token) = token {
            if token.kind() == SyntaxKind::NUMBER {
                println!("Number: {} at {:?}", token.text(), token.text_range());
            }
        }
    }
}
```

### Tree Modification

```rust
// Modifying trees creates new green trees with sharing
fn modify_tree(original: SyntaxNode<MyLang>) -> SyntaxNode<MyLang> {
    let mut builder = GreenNodeBuilder::new();
    
    // Copy most of the tree, but change specific parts
    builder.start_node(original.kind().into());
    
    for child in original.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(node) => {
                if should_modify(&node) {
                    // Build a modified version
                    let modified = modify_node(node);
                    builder.add_child(modified.green().clone().into());
                } else {
                    // Reuse the existing green node
                    builder.add_child(node.green().clone().into());
                }
            }
            rowan::NodeOrToken::Token(token) => {
                builder.token(token.kind().into(), token.text());
            }
        }
    }
    
    builder.finish_node();
    let green = builder.finish();
    SyntaxNode::new_root(green)
}
```

## Best Practices

### When to Use Green vs Red

**Use Green Trees for:**
- Long-term storage
- Serialization
- Structural comparisons
- Building new trees

**Use Red Trees for:**
- Navigation and analysis
- AST construction
- IDE features (go-to-definition, etc.)
- Tree queries

### Memory Management

```rust
// Good: Share green trees
let green = build_tree();
let syntax1 = SyntaxNode::new_root(green.clone());
let syntax2 = SyntaxNode::new_root(green.clone());

// Good: Cache frequently accessed nodes
struct Parser {
    cached_nodes: HashMap<SyntaxKind, GreenNode>,
}

// Avoid: Creating many red trees for the same green tree
// Each red tree has its own navigation state
```

### Performance Tips

1. **Reuse green trees**: Clone green nodes instead of rebuilding
2. **Cache common patterns**: Store frequently used subtrees
3. **Batch modifications**: Build new trees in single passes
4. **Use cursors for traversal**: More efficient than recursive navigation

## Integration with Incremental Parsing

The green/red architecture enables efficient incremental parsing:

```rust
// Incremental parsing reuses unchanged subtrees
fn incremental_parse(
    old_tree: GreenNode,
    edit: TextEdit,
) -> GreenNode {
    // 1. Find the minimal affected range
    let affected_range = find_affected_range(&old_tree, &edit);
    
    // 2. Reparse only the affected portion
    let new_subtree = parse_range(affected_range);
    
    // 3. Build new tree reusing unchanged parts
    let mut builder = GreenNodeBuilder::new();
    
    // Reuse prefix
    for child in old_tree.children().take_while(|c| !overlaps(c, affected_range)) {
        builder.add_child(child.clone());
    }
    
    // Add reparsed content
    builder.add_child(new_subtree);
    
    // Reuse suffix
    for child in old_tree.children().skip_while(|c| overlaps(c, affected_range)) {
        builder.add_child(child.clone());
    }
    
    builder.finish()
}
```

## Summary

The green/red architecture is Rowan's key innovation:

- **Green trees** provide immutable, memory-efficient storage with structural sharing
- **Red trees** provide convenient navigation APIs with parent pointers and absolute positions
- **Separation of concerns** enables both efficiency and usability
- **Incremental parsing** becomes natural through green tree reuse
- **Memory usage** scales with unique content, not total tree size

This architecture makes Rowan suitable for large-scale language processing where both performance and convenience matter.