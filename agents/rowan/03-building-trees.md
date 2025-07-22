# Building Syntax Trees

This guide covers how to construct syntax trees using Rowan's `GreenNodeBuilder`. The builder provides a stack-based API for incrementally constructing immutable green trees.

## GreenNodeBuilder Overview

The `GreenNodeBuilder` is the primary tool for constructing syntax trees. It maintains a stack of in-progress nodes and efficiently builds the final immutable tree.

```rust
use rowan::{GreenNodeBuilder, SyntaxKind};

let mut builder = GreenNodeBuilder::new();
```

### Key Concepts

- **Stack-based**: Uses a stack to track nested nodes
- **Incremental**: Build trees piece by piece
- **Efficient**: Reuses common subtrees through caching
- **Lossless**: Preserves all source text including whitespace

## Basic Building Operations

### Starting and Finishing Nodes

```rust
// Start a new node
builder.start_node(SyntaxKind::FUNCTION.into());

// Add content (tokens and child nodes)
builder.token(SyntaxKind::FN_KW.into(), "fn");
builder.token(SyntaxKind::IDENT.into(), "foo");

// Finish the current node
builder.finish_node();
```

### Adding Tokens

```rust
// Add individual tokens
builder.token(SyntaxKind::IDENT.into(), "variable");
builder.token(SyntaxKind::WHITESPACE.into(), " ");
builder.token(SyntaxKind::EQUALS.into(), "=");
builder.token(SyntaxKind::WHITESPACE.into(), " ");
builder.token(SyntaxKind::NUMBER.into(), "42");
builder.token(SyntaxKind::SEMICOLON.into(), ";");
```

### Completing the Tree

```rust
// Finish building and get the final green tree
let green_tree = builder.finish();
```

## Complete Example: Building a Function

```rust
use rowan::{GreenNodeBuilder, Language, SyntaxNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
enum SyntaxKind {
    // Tokens
    FN_KW = 0,
    IDENT,
    L_PAREN,
    R_PAREN,
    L_BRACE,
    R_BRACE,
    WHITESPACE,
    
    // Nodes
    FUNCTION,
    PARAM_LIST,
    BLOCK,
    ROOT,
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

fn build_function() -> SyntaxNode<Lang> {
    let mut builder = GreenNodeBuilder::new();
    
    // Root node
    builder.start_node(SyntaxKind::ROOT.into());
    
    // Function node
    builder.start_node(SyntaxKind::FUNCTION.into());
    
    // fn keyword
    builder.token(SyntaxKind::FN_KW.into(), "fn");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // Function name
    builder.token(SyntaxKind::IDENT.into(), "hello");
    
    // Parameter list
    builder.start_node(SyntaxKind::PARAM_LIST.into());
    builder.token(SyntaxKind::L_PAREN.into(), "(");
    builder.token(SyntaxKind::R_PAREN.into(), ")");
    builder.finish_node();
    
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // Function body
    builder.start_node(SyntaxKind::BLOCK.into());
    builder.token(SyntaxKind::L_BRACE.into(), "{");
    builder.token(SyntaxKind::R_BRACE.into(), "}");
    builder.finish_node();
    
    builder.finish_node(); // FUNCTION
    builder.finish_node(); // ROOT
    
    let green = builder.finish();
    SyntaxNode::new_root(green)
}
```

## Advanced Building Techniques

### Checkpoints for Conditional Wrapping

Checkpoints allow you to conditionally wrap previously added content in a new node. This is useful for handling operator precedence and optional syntax elements.

```rust
use rowan::Checkpoint;

fn parse_expression_with_precedence(builder: &mut GreenNodeBuilder) {
    // Parse the left operand
    let checkpoint = builder.checkpoint();
    parse_primary_expression(builder);
    
    // Check if there's an operator
    if let Some(op) = peek_operator() {
        // Wrap the left operand in a binary expression
        builder.start_node_at(checkpoint, SyntaxKind::BINARY_EXPR.into());
        
        // Add the operator
        builder.token(op.kind().into(), op.text());
        
        // Parse the right operand
        parse_expression_with_precedence(builder);
        
        builder.finish_node();
    }
    // If no operator, the checkpoint is simply ignored
}
```

### Example: Parsing Arithmetic Expressions

```rust
fn parse_arithmetic() -> SyntaxNode<Lang> {
    let mut builder = GreenNodeBuilder::new();
    
    builder.start_node(SyntaxKind::ROOT.into());
    
    // Parse "2 + 3 * 4"
    let checkpoint = builder.checkpoint();
    
    // First number
    builder.token(SyntaxKind::NUMBER.into(), "2");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // First operator (+)
    builder.start_node_at(checkpoint, SyntaxKind::BINARY_EXPR.into());
    builder.token(SyntaxKind::PLUS.into(), "+");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // Right side: "3 * 4" (higher precedence)
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
    SyntaxNode::new_root(green)
}
```

## Node Caching for Memory Efficiency

The builder can reuse a `NodeCache` to share common subtrees between different trees:

```rust
use rowan::NodeCache;

fn build_multiple_trees_efficiently() {
    let mut cache = NodeCache::default();
    
    // Build first tree
    let tree1 = {
        let mut builder = GreenNodeBuilder::with_cache(&mut cache);
        // ... build tree
        builder.finish()
    };
    
    // Build second tree, reusing cached nodes
    let tree2 = {
        let mut builder = GreenNodeBuilder::with_cache(&mut cache);
        // ... build tree (common subtrees will be shared)
        builder.finish()
    };
}
```

### Cache Benefits

```rust
// Example: Common patterns are cached
fn build_many_functions(cache: &mut NodeCache) -> Vec<GreenNode> {
    let mut trees = Vec::new();
    
    for name in ["foo", "bar", "baz"] {
        let mut builder = GreenNodeBuilder::with_cache(cache);
        
        builder.start_node(SyntaxKind::FUNCTION.into());
        builder.token(SyntaxKind::FN_KW.into(), "fn");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        builder.token(SyntaxKind::IDENT.into(), name);
        
        // This parameter list will be cached and shared
        builder.start_node(SyntaxKind::PARAM_LIST.into());
        builder.token(SyntaxKind::L_PAREN.into(), "(");
        builder.token(SyntaxKind::R_PAREN.into(), ")");
        builder.finish_node();
        
        // This empty block will be cached and shared
        builder.start_node(SyntaxKind::BLOCK.into());
        builder.token(SyntaxKind::L_BRACE.into(), "{");
        builder.token(SyntaxKind::R_BRACE.into(), "}");
        builder.finish_node();
        
        builder.finish_node();
        trees.push(builder.finish());
    }
    
    trees
}
```

## Error Recovery in Building

When building trees from potentially invalid input, you can include error nodes:

```rust
fn build_with_errors(tokens: &[(SyntaxKind, &str)]) -> SyntaxNode<Lang> {
    let mut builder = GreenNodeBuilder::new();
    
    builder.start_node(SyntaxKind::ROOT.into());
    
    for (kind, text) in tokens {
        match kind {
            SyntaxKind::INVALID => {
                // Wrap invalid tokens in error nodes
                builder.start_node(SyntaxKind::ERROR.into());
                builder.token(*kind, text);
                builder.finish_node();
            }
            _ => {
                builder.token(*kind, text);
            }
        }
    }
    
    builder.finish_node();
    let green = builder.finish();
    SyntaxNode::new_root(green)
}
```

## Building from Existing Trees

You can build new trees by combining parts of existing trees:

```rust
fn combine_trees(tree1: &SyntaxNode<Lang>, tree2: &SyntaxNode<Lang>) -> SyntaxNode<Lang> {
    let mut builder = GreenNodeBuilder::new();
    
    builder.start_node(SyntaxKind::ROOT.into());
    
    // Add first tree's content
    for element in tree1.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(node) => {
                // Reuse the green node directly
                builder.add_child(node.green().clone().into());
            }
            rowan::NodeOrToken::Token(token) => {
                builder.token(token.kind().into(), token.text());
            }
        }
    }
    
    // Add separator
    builder.token(SyntaxKind::WHITESPACE.into(), "\n");
    
    // Add second tree's content
    for element in tree2.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(node) => {
                builder.add_child(node.green().clone().into());
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

### 1. Always Match start_node/finish_node

```rust
// Good: Balanced calls
builder.start_node(SyntaxKind::FUNCTION.into());
// ... add content
builder.finish_node();

// Bad: Unbalanced calls will panic
builder.start_node(SyntaxKind::FUNCTION.into());
// Missing finish_node() call
```

### 2. Use Checkpoints for Optional Wrapping

```rust
// Good: Use checkpoints for conditional structure
let checkpoint = builder.checkpoint();
parse_primary();
if has_operator() {
    builder.start_node_at(checkpoint, SyntaxKind::BINARY_EXPR.into());
    parse_operator();
    parse_primary();
    builder.finish_node();
}

// Avoid: Building and then restructuring
```

### 3. Preserve All Source Text

```rust
// Good: Include all whitespace and comments
builder.token(SyntaxKind::IDENT.into(), "foo");
builder.token(SyntaxKind::WHITESPACE.into(), " ");
builder.token(SyntaxKind::COMMENT.into(), "// comment");
builder.token(SyntaxKind::WHITESPACE.into(), "\n");

// Bad: Losing source information
builder.token(SyntaxKind::IDENT.into(), "foo");
// Missing whitespace and comments
```

### 4. Use Caching for Performance

```rust
// Good: Reuse cache across multiple builds
let mut cache = NodeCache::default();
for source in sources {
    let mut builder = GreenNodeBuilder::with_cache(&mut cache);
    // ... build tree
}

// Less efficient: New cache for each build
for source in sources {
    let mut builder = GreenNodeBuilder::new();
    // ... build tree
}
```

### 5. Handle Errors Gracefully

```rust
// Good: Include error recovery
fn parse_statement(builder: &mut GreenNodeBuilder, tokens: &[Token]) {
    match tokens[0].kind {
        SyntaxKind::LET => parse_let_statement(builder, tokens),
        SyntaxKind::IF => parse_if_statement(builder, tokens),
        _ => {
            // Create error node for unexpected tokens
            builder.start_node(SyntaxKind::ERROR.into());
            builder.token(tokens[0].kind.into(), &tokens[0].text);
            builder.finish_node();
        }
    }
}
```

## Performance Considerations

### Memory Usage

- **Structural sharing**: Identical subtrees share memory
- **Caching**: Common patterns are cached and reused
- **Compact representation**: Optimized internal layout

### Build Performance

- **Linear complexity**: Building is O(n) in tree size
- **Incremental**: Only build what's needed
- **Cache hits**: Reused nodes are very fast

### Example: Measuring Cache Effectiveness

```rust
use rowan::NodeCache;

fn measure_cache_effectiveness() {
    let mut cache = NodeCache::default();
    
    // Build many similar trees
    for i in 0..1000 {
        let mut builder = GreenNodeBuilder::with_cache(&mut cache);
        
        // Build a function with common structure
        builder.start_node(SyntaxKind::FUNCTION.into());
        builder.token(SyntaxKind::FN_KW.into(), "fn");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        builder.token(SyntaxKind::IDENT.into(), &format!("func_{}", i));
        
        // Common empty parameter list (will be cached)
        builder.start_node(SyntaxKind::PARAM_LIST.into());
        builder.token(SyntaxKind::L_PAREN.into(), "(");
        builder.token(SyntaxKind::R_PAREN.into(), ")");
        builder.finish_node();
        
        // Common empty body (will be cached)
        builder.start_node(SyntaxKind::BLOCK.into());
        builder.token(SyntaxKind::L_BRACE.into(), "{");
        builder.token(SyntaxKind::R_BRACE.into(), "}");
        builder.finish_node();
        
        builder.finish_node();
        let _tree = builder.finish();
    }
    
    // The cache now contains shared subtrees for parameter lists and blocks
    println!("Cache contains {} unique nodes", cache.len());
}
```

## Integration with Parsers

The builder integrates naturally with recursive descent parsers:

```rust
struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    builder: GreenNodeBuilder<'a>,
}

impl<'a> Parser<'a> {
    fn parse_function(&mut self) {
        self.builder.start_node(SyntaxKind::FUNCTION.into());
        
        self.expect(SyntaxKind::FN_KW);
        self.skip_whitespace();
        self.expect(SyntaxKind::IDENT);
        
        self.parse_parameter_list();
        self.skip_whitespace();
        self.parse_block();
        
        self.builder.finish_node();
    }
    
    fn expect(&mut self, kind: SyntaxKind) {
        if self.current_kind() == Some(kind) {
            let token = &self.tokens[self.pos];
            self.builder.token(kind.into(), &token.text);
            self.pos += 1;
        } else {
            // Error recovery
            self.builder.start_node(SyntaxKind::ERROR.into());
            if let Some(token) = self.current_token() {
                self.builder.token(token.kind.into(), &token.text);
                self.pos += 1;
            }
            self.builder.finish_node();
        }
    }
}
```

Building syntax trees with Rowan is straightforward once you understand the stack-based approach and the power of checkpoints for conditional structure. The key is to preserve all source information while building efficient, shareable tree structures.