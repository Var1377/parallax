# Complete API Reference

This comprehensive reference covers all public APIs in Rowan 0.16, organized by functionality with detailed examples and usage patterns.

## Core Types

### Language Trait

The `Language` trait defines how your syntax kinds map to Rowan's internal representation.

```rust
pub trait Language: Sized + Copy + fmt::Debug + Eq + Ord + std::hash::Hash {
    type Kind: Sized + Copy + fmt::Debug + Eq + Ord + std::hash::Hash;

    fn kind_from_raw(raw: SyntaxKind) -> Self::Kind;
    fn kind_to_raw(kind: Self::Kind) -> SyntaxKind;
}
```

**Example Implementation:**
```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
enum SyntaxKind {
    IDENT = 0,
    NUMBER,
    PLUS,
    // ... other kinds
    ROOT,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum MyLang {}

impl Language for MyLang {
    type Kind = SyntaxKind;
    
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute(raw.0 as u8) }
    }
    
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}
```

### SyntaxKind

Internal representation of syntax kinds.

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxKind(pub u16);
```

**Usage:**
```rust
let kind = SyntaxKind(42);
let from_enum: rowan::SyntaxKind = SyntaxKind::IDENT.into();
```

## Green Tree API

### GreenNode

Immutable tree nodes that form the structural backbone of syntax trees.

```rust
impl GreenNode {
    // Access properties
    pub fn kind(&self) -> SyntaxKind;
    pub fn text_len(&self) -> TextSize;
    pub fn children(&self) -> Children<'_>;
    
    // Create new trees
    pub fn new(kind: SyntaxKind, children: impl IntoIterator<Item = GreenElement>) -> GreenNode;
}
```

**Examples:**
```rust
// Access node properties
let node: GreenNode = /* ... */;
println!("Kind: {:?}", node.kind());
println!("Text length: {:?}", node.text_len());

// Iterate over children
for child in node.children() {
    match child {
        NodeOrToken::Node(node) => println!("Child node: {:?}", node.kind()),
        NodeOrToken::Token(token) => println!("Child token: {:?}", token.text()),
    }
}

// Create a new node manually
let children = vec![
    GreenToken::new(SyntaxKind::IDENT.into(), "foo").into(),
    GreenToken::new(SyntaxKind::PLUS.into(), "+").into(),
    GreenToken::new(SyntaxKind::NUMBER.into(), "42").into(),
];
let expr_node = GreenNode::new(SyntaxKind::EXPR.into(), children);
```

### GreenToken

Immutable leaf nodes containing text.

```rust
impl GreenToken {
    pub fn new(kind: SyntaxKind, text: &str) -> GreenToken;
    pub fn kind(&self) -> SyntaxKind;
    pub fn text(&self) -> &str;
}
```

**Examples:**
```rust
// Create tokens
let ident = GreenToken::new(SyntaxKind::IDENT.into(), "variable");
let number = GreenToken::new(SyntaxKind::NUMBER.into(), "42");

// Access token properties
println!("Token kind: {:?}", ident.kind());
println!("Token text: {}", ident.text());
```

### GreenNodeBuilder

Builder for constructing green trees incrementally.

```rust
impl GreenNodeBuilder<'_> {
    // Construction
    pub fn new() -> GreenNodeBuilder<'static>;
    pub fn with_cache(cache: &mut NodeCache) -> GreenNodeBuilder<'_>;
    
    // Building operations
    pub fn start_node(&mut self, kind: SyntaxKind);
    pub fn finish_node(&mut self);
    pub fn token(&mut self, kind: SyntaxKind, text: &str);
    
    // Advanced operations
    pub fn checkpoint(&self) -> Checkpoint;
    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind);
    
    // Completion
    pub fn finish(self) -> GreenNode;
}
```

**Examples:**
```rust
// Basic building
let mut builder = GreenNodeBuilder::new();

builder.start_node(SyntaxKind::EXPR.into());
builder.token(SyntaxKind::NUMBER.into(), "42");
builder.token(SyntaxKind::PLUS.into(), "+");
builder.token(SyntaxKind::NUMBER.into(), "13");
builder.finish_node();

let green = builder.finish();

// Using checkpoints for conditional wrapping
let mut builder = GreenNodeBuilder::new();
let checkpoint = builder.checkpoint();

builder.token(SyntaxKind::NUMBER.into(), "42");

// Conditionally wrap in expression
if should_wrap_in_expr() {
    builder.start_node_at(checkpoint, SyntaxKind::EXPR.into());
    builder.token(SyntaxKind::PLUS.into(), "+");
    builder.token(SyntaxKind::NUMBER.into(), "13");
    builder.finish_node();
}

let green = builder.finish();

// Using cache for efficiency
let mut cache = NodeCache::default();
let mut builder = GreenNodeBuilder::with_cache(&mut cache);
// ... build multiple trees that can share common subtrees
```

### Checkpoint

Marker for conditional node wrapping.

```rust
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(/* private */);
```

**Usage:**
```rust
let mut builder = GreenNodeBuilder::new();

// Save position
let checkpoint = builder.checkpoint();

// Add some content
builder.token(SyntaxKind::IDENT.into(), "foo");

// Conditionally wrap
if needs_wrapping {
    builder.start_node_at(checkpoint, SyntaxKind::PATH.into());
    builder.finish_node();
}
```

### NodeCache

Cache for deduplicating identical nodes and tokens.

```rust
impl NodeCache {
    pub fn default() -> Self;
    // Internal methods for caching - used by GreenNodeBuilder
}
```

**Usage:**
```rust
let mut cache = NodeCache::default();

// Use cache across multiple builds
for input in inputs {
    let mut builder = GreenNodeBuilder::with_cache(&mut cache);
    let tree = parse_with_builder(&mut builder, input);
    // Common subtrees will be shared
}
```

## Red Tree API

### SyntaxNode<L>

Mutable view over green trees with parent pointers and absolute positions.

```rust
impl<L: Language> SyntaxNode<L> {
    // Creation
    pub fn new_root(green: GreenNode) -> SyntaxNode<L>;
    pub fn new_root_mut(green: GreenNode) -> SyntaxNode<L>;
    
    // Properties
    pub fn kind(&self) -> L::Kind;
    pub fn text_range(&self) -> TextRange;
    pub fn text(&self) -> SyntaxText;
    pub fn index(&self) -> usize;
    
    // Tree access
    pub fn green(&self) -> Cow<'_, GreenNodeData>;
    pub fn parent(&self) -> Option<SyntaxNode<L>>;
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<L>>;
    
    // Children access
    pub fn children(&self) -> SyntaxNodeChildren<L>;
    pub fn children_with_tokens(&self) -> SyntaxElementChildren<L>;
    pub fn first_child(&self) -> Option<SyntaxNode<L>>;
    pub fn last_child(&self) -> Option<SyntaxNode<L>>;
    pub fn first_child_or_token(&self) -> Option<SyntaxElement<L>>;
    pub fn last_child_or_token(&self) -> Option<SyntaxElement<L>>;
    
    // Filtered access
    pub fn first_child_by_kind(&self, matcher: &impl Fn(L::Kind) -> bool) -> Option<SyntaxNode<L>>;
    pub fn first_child_or_token_by_kind(&self, matcher: &impl Fn(L::Kind) -> bool) -> Option<SyntaxElement<L>>;
    
    // Sibling navigation
    pub fn next_sibling(&self) -> Option<SyntaxNode<L>>;
    pub fn prev_sibling(&self) -> Option<SyntaxNode<L>>;
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<L>>;
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<L>>;
    pub fn next_sibling_by_kind(&self, matcher: &impl Fn(L::Kind) -> bool) -> Option<SyntaxNode<L>>;
    pub fn next_sibling_or_token_by_kind(&self, matcher: &impl Fn(L::Kind) -> bool) -> Option<SyntaxElement<L>>;
    
    // Token access
    pub fn first_token(&self) -> Option<SyntaxToken<L>>;
    pub fn last_token(&self) -> Option<SyntaxToken<L>>;
    
    // Traversal
    pub fn siblings(&self, direction: Direction) -> impl Iterator<Item = SyntaxNode<L>>;
    pub fn siblings_with_tokens(&self, direction: Direction) -> impl Iterator<Item = SyntaxElement<L>>;
    pub fn descendants(&self) -> impl Iterator<Item = SyntaxNode<L>>;
    pub fn descendants_with_tokens(&self) -> impl Iterator<Item = SyntaxElement<L>>;
    pub fn preorder(&self) -> Preorder<L>;
    pub fn preorder_with_tokens(&self) -> PreorderWithTokens<L>;
    
    // Positional queries
    pub fn token_at_offset(&self, offset: TextSize) -> TokenAtOffset<SyntaxToken<L>>;
    pub fn covering_element(&self, range: TextRange) -> SyntaxElement<L>;
    pub fn child_or_token_at_range(&self, range: TextRange) -> Option<SyntaxElement<L>>;
    
    // Tree modification
    pub fn replace_with(&self, replacement: GreenNode) -> GreenNode;
    pub fn clone_subtree(&self) -> SyntaxNode<L>;
    pub fn clone_for_update(&self) -> SyntaxNode<L>;
    pub fn detach(&self);
    pub fn splice_children<I: IntoIterator<Item = SyntaxElement<L>>>(&self, to_delete: Range<usize>, to_insert: I);
    
    // Mutability
    pub fn is_mutable(&self) -> bool;
}
```

**Examples:**

**Basic Navigation:**
```rust
let root: SyntaxNode<MyLang> = SyntaxNode::new_root(green);

// Access properties
println!("Root kind: {:?}", root.kind());
println!("Root text: {}", root.text());
println!("Root range: {:?}", root.text_range());

// Navigate to children
for child in root.children() {
    println!("Child: {:?} at {:?}", child.kind(), child.text_range());
}

// Navigate to parent
if let Some(parent) = root.parent() {
    println!("Parent: {:?}", parent.kind());
}
```

**Tree Traversal:**
```rust
// Find all identifiers
let identifiers: Vec<_> = root
    .descendants_with_tokens()
    .filter_map(|element| element.into_token())
    .filter(|token| token.kind() == SyntaxKind::IDENT)
    .map(|token| token.text().to_string())
    .collect();

// Preorder traversal with enter/leave events
for event in root.preorder_with_tokens() {
    match event {
        WalkEvent::Enter(element) => {
            println!("Entering: {:?}", element.kind());
        }
        WalkEvent::Leave(element) => {
            println!("Leaving: {:?}", element.kind());
        }
    }
}
```

**Positional Queries:**
```rust
use text_size::TextSize;

// Find token at specific position
let offset = TextSize::from(10);
match root.token_at_offset(offset) {
    TokenAtOffset::None => println!("No token at offset"),
    TokenAtOffset::Single(token) => println!("Token: {}", token.text()),
    TokenAtOffset::Between(left, right) => {
        println!("Between: {} and {}", left.text(), right.text());
    }
}

// Find element covering a range
let range = TextRange::new(TextSize::from(5), TextSize::from(15));
let covering = root.covering_element(range);
println!("Covering element: {:?}", covering.kind());
```

**Filtered Searches:**
```rust
// Find first function
let first_function = root.first_child_by_kind(&|kind| kind == SyntaxKind::FUNCTION);

// Find all functions
let functions: Vec<_> = root
    .descendants()
    .filter(|node| node.kind() == SyntaxKind::FUNCTION)
    .collect();

// Find next sibling of specific kind
let next_stmt = node.next_sibling_by_kind(&|kind| {
    matches!(kind, SyntaxKind::LET_STMT | SyntaxKind::EXPR_STMT)
});
```

### SyntaxToken<L>

Wrapper around green tokens with red tree navigation.

```rust
impl<L: Language> SyntaxToken<L> {
    // Properties
    pub fn kind(&self) -> L::Kind;
    pub fn text(&self) -> &str;
    pub fn text_range(&self) -> TextRange;
    pub fn index(&self) -> usize;
    
    // Tree access
    pub fn green(&self) -> &GreenTokenData;
    pub fn parent(&self) -> Option<SyntaxNode<L>>;
    pub fn parent_ancestors(&self) -> impl Iterator<Item = SyntaxNode<L>>;
    
    // Sibling navigation
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<L>>;
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<L>>;
    
    // Tree modification
    pub fn replace_with(&self, new_token: GreenToken) -> GreenNode;
    pub fn detach(&self);
}
```

**Examples:**
```rust
// Access token properties
let token: SyntaxToken<MyLang> = /* ... */;
println!("Token: {} ({:?}) at {:?}", token.text(), token.kind(), token.text_range());

// Navigate to parent
if let Some(parent) = token.parent() {
    println!("Token parent: {:?}", parent.kind());
}

// Find containing function
let containing_function = token
    .parent_ancestors()
    .find(|node| node.kind() == SyntaxKind::FUNCTION);
```

### SyntaxElement<L>

Union type representing either a node or token.

```rust
pub type SyntaxElement<L> = NodeOrToken<SyntaxNode<L>, SyntaxToken<L>>;
```

**Examples:**
```rust
// Pattern match on element
match element {
    NodeOrToken::Node(node) => {
        println!("Node: {:?}", node.kind());
    }
    NodeOrToken::Token(token) => {
        println!("Token: {} ({:?})", token.text(), token.kind());
    }
}

// Convert to specific type
if let Some(node) = element.into_node() {
    // Work with node
}

if let Some(token) = element.into_token() {
    // Work with token
}

// Access common properties
println!("Element kind: {:?}", element.kind());
println!("Element range: {:?}", element.text_range());
```

## Utility Types

### NodeOrToken<N, T>

Generic union type for nodes and tokens.

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeOrToken<N, T> {
    Node(N),
    Token(T),
}

impl<N, T> NodeOrToken<N, T> {
    pub fn into_node(self) -> Option<N>;
    pub fn into_token(self) -> Option<T>;
    pub fn as_node(&self) -> Option<&N>;
    pub fn as_token(&self) -> Option<&T>;
}
```

### Direction

Enumeration for traversal direction.

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    Next,
    Prev,
}
```

**Usage:**
```rust
// Traverse siblings in both directions
for sibling in node.siblings(Direction::Next) {
    println!("Next sibling: {:?}", sibling.kind());
}

for sibling in node.siblings(Direction::Prev) {
    println!("Previous sibling: {:?}", sibling.kind());
}
```

### WalkEvent<T>

Event type for preorder traversal.

```rust
#[derive(Debug, Copy, Clone)]
pub enum WalkEvent<T> {
    Enter(T),
    Leave(T),
}

impl<T> WalkEvent<T> {
    pub fn map<F: FnOnce(T) -> U, U>(self, f: F) -> WalkEvent<U>;
}
```

**Usage:**
```rust
for event in node.preorder() {
    match event {
        WalkEvent::Enter(node) => {
            println!("Entering: {:?}", node.kind());
        }
        WalkEvent::Leave(node) => {
            println!("Leaving: {:?}", node.kind());
        }
    }
}
```

### TokenAtOffset<T>

Result type for positional token queries.

```rust
#[derive(Clone, Debug)]
pub enum TokenAtOffset<T> {
    None,
    Single(T),
    Between(T, T),
}

impl<T> TokenAtOffset<T> {
    pub fn map<F: Fn(T) -> U, U>(self, f: F) -> TokenAtOffset<U>;
    pub fn right_biased(self) -> Option<T>;
    pub fn left_biased(self) -> Option<T>;
}
```

**Usage:**
```rust
let result = node.token_at_offset(offset);

// Handle all cases
match result {
    TokenAtOffset::None => println!("No token at offset"),
    TokenAtOffset::Single(token) => println!("Single token: {}", token.text()),
    TokenAtOffset::Between(left, right) => {
        println!("Between tokens: {} | {}", left.text(), right.text());
    }
}

// Get preferred token
if let Some(token) = result.right_biased() {
    println!("Preferred token: {}", token.text());
}

// Iterate over all tokens
for token in result {
    println!("Token: {}", token.text());
}
```

## Text Handling

### SyntaxText

Efficient text representation for syntax trees.

```rust
impl SyntaxText {
    // Access methods
    pub fn len(&self) -> TextSize;
    pub fn is_empty(&self) -> bool;
    
    // Conversion
    pub fn to_string(&self) -> String;
}

impl fmt::Display for SyntaxText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}
```

**Usage:**
```rust
let text = node.text();
println!("Node text: {}", text);
println!("Text length: {:?}", text.len());

// Convert to string when needed
let string_text = text.to_string();
```

### Text Size and Range Types

Re-exported from `text-size` crate:

```rust
pub use text_size::{TextLen, TextRange, TextSize};
```

**Usage:**
```rust
use text_size::{TextSize, TextRange};

// Create text positions
let start = TextSize::from(10);
let end = TextSize::from(20);
let range = TextRange::new(start, end);

// Range operations
println!("Range length: {:?}", range.len());
println!("Range contains offset 15: {}", range.contains(TextSize::from(15)));

// Create ranges from strings
let text = "hello world";
let range = TextRange::up_to(TextSize::of(text));
```

## Iterator Types

### SyntaxNodeChildren<L>

Iterator over child nodes.

```rust
impl<L: Language> Iterator for SyntaxNodeChildren<L> {
    type Item = SyntaxNode<L>;
}
```

### SyntaxElementChildren<L>

Iterator over child nodes and tokens.

```rust
impl<L: Language> Iterator for SyntaxElementChildren<L> {
    type Item = SyntaxElement<L>;
}
```

### Preorder<L> and PreorderWithTokens<L>

Iterators for preorder traversal.

```rust
impl<L: Language> Iterator for Preorder<L> {
    type Item = WalkEvent<SyntaxNode<L>>;
}

impl<L: Language> Iterator for PreorderWithTokens<L> {
    type Item = WalkEvent<SyntaxElement<L>>;
}
```

**Usage:**
```rust
// Iterate over child nodes only
for child in node.children() {
    println!("Child node: {:?}", child.kind());
}

// Iterate over all children (nodes and tokens)
for element in node.children_with_tokens() {
    match element {
        NodeOrToken::Node(node) => println!("Child node: {:?}", node.kind()),
        NodeOrToken::Token(token) => println!("Child token: {}", token.text()),
    }
}

// Preorder traversal
for event in node.preorder() {
    if let WalkEvent::Enter(node) = event {
        println!("Visiting node: {:?}", node.kind());
    }
}
```

## Complete Example

Here's a comprehensive example showing many API features:

```rust
use rowan::{GreenNodeBuilder, Language, SyntaxNode, TextSize, WalkEvent};

// Define language
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
enum SyntaxKind {
    IDENT = 0, NUMBER, PLUS, MINUS, STAR, SLASH,
    L_PAREN, R_PAREN, WHITESPACE,
    EXPR, BINARY_EXPR, LITERAL, ROOT,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        rowan::SyntaxKind(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}

impl Language for Lang {
    type Kind = SyntaxKind;
    
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

type SyntaxNode = rowan::SyntaxNode<Lang>;
type SyntaxToken = rowan::SyntaxToken<Lang>;

fn main() {
    // Build a tree: "2 + 3 * 4"
    let mut builder = GreenNodeBuilder::new();
    
    builder.start_node(SyntaxKind::ROOT.into());
    
    // Build binary expression with precedence
    let checkpoint = builder.checkpoint();
    builder.token(SyntaxKind::NUMBER.into(), "2");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    builder.start_node_at(checkpoint, SyntaxKind::BINARY_EXPR.into());
    builder.token(SyntaxKind::PLUS.into(), "+");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // Right side with higher precedence
    let right_checkpoint = builder.checkpoint();
    builder.token(SyntaxKind::NUMBER.into(), "3");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    builder.start_node_at(right_checkpoint, SyntaxKind::BINARY_EXPR.into());
    builder.token(SyntaxKind::STAR.into(), "*");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.token(SyntaxKind::NUMBER.into(), "4");
    builder.finish_node(); // 3 * 4
    
    builder.finish_node(); // 2 + (3 * 4)
    builder.finish_node(); // ROOT
    
    let green = builder.finish();
    let root = SyntaxNode::new_root(green);
    
    // Demonstrate API usage
    println!("=== Tree Structure ===");
    println!("{:#?}", root);
    
    println!("\n=== Tree Properties ===");
    println!("Root text: '{}'", root.text());
    println!("Root range: {:?}", root.text_range());
    println!("Root kind: {:?}", root.kind());
    
    println!("\n=== Children ===");
    for child in root.children() {
        println!("Child: {:?} at {:?}", child.kind(), child.text_range());
    }
    
    println!("\n=== All Elements ===");
    for element in root.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(node) => {
                println!("Node: {:?} at {:?}", node.kind(), node.text_range());
            }
            rowan::NodeOrToken::Token(token) => {
                println!("Token: '{}' ({:?}) at {:?}", 
                    token.text(), token.kind(), token.text_range());
            }
        }
    }
    
    println!("\n=== Find All Numbers ===");
    let numbers: Vec<_> = root
        .descendants_with_tokens()
        .filter_map(|element| element.into_token())
        .filter(|token| token.kind() == SyntaxKind::NUMBER)
        .map(|token| token.text())
        .collect();
    println!("Numbers found: {:?}", numbers);
    
    println!("\n=== Token at Position ===");
    let offset = TextSize::from(4); // Position of '+'
    match root.token_at_offset(offset) {
        rowan::TokenAtOffset::Single(token) => {
            println!("Token at offset {}: '{}' ({:?})", 
                offset, token.text(), token.kind());
        }
        _ => println!("No single token at offset {}", offset),
    }
    
    println!("\n=== Preorder Traversal ===");
    for event in root.preorder_with_tokens() {
        match event {
            WalkEvent::Enter(element) => {
                let indent = "  ".repeat(0); // Would track depth in real code
                match element {
                    rowan::NodeOrToken::Node(node) => {
                        println!("{}Enter {:?}", indent, node.kind());
                    }
                    rowan::NodeOrToken::Token(token) => {
                        println!("{}Token '{}' ({:?})", indent, token.text(), token.kind());
                    }
                }
            }
            WalkEvent::Leave(element) => {
                if let rowan::NodeOrToken::Node(node) = element {
                    let indent = "  ".repeat(0);
                    println!("{}Leave {:?}", indent, node.kind());
                }
            }
        }
    }
}
```

This API reference covers all major functionality in Rowan 0.16. The key patterns are:

1. **Use the Language trait** to define your syntax kinds
2. **Build with GreenNodeBuilder** for constructing trees
3. **Navigate with SyntaxNode** for tree analysis
4. **Use iterators** for efficient traversal
5. **Handle positions** with TextSize and TextRange
6. **Pattern match** on NodeOrToken for element handling

The API is designed to be both powerful and safe, leveraging Rust's type system to prevent common tree manipulation errors while providing efficient access to tree structure and content.