# Getting Started with Rowan

Rowan is a library for building lossless syntax trees. Unlike traditional ASTs that discard formatting and comments, Rowan preserves every character from the source text while providing efficient tree operations.

## Core Concepts

### What is a Lossless Syntax Tree?

A lossless syntax tree preserves all information from the source text:
- **Tokens**: Individual lexical elements (keywords, identifiers, operators, literals)
- **Whitespace**: Spaces, tabs, newlines
- **Comments**: Both line and block comments
- **Structure**: Hierarchical organization of syntax elements

### Why Lossless Trees?

Traditional ASTs lose information during parsing, making them unsuitable for:
- **Code formatters**: Need to preserve spacing and comments
- **Language servers**: Require precise source locations
- **Refactoring tools**: Must maintain code structure
- **Incremental parsing**: Need to track changes precisely

## Basic Setup

Add Rowan to your `Cargo.toml`:

```toml
[dependencies]
rowan = "0.16"
text-size = "1.1"  # For text ranges and positions
```

## Defining Your Language

Every Rowan tree is parameterized by a `Language` trait that defines the syntax kinds:

```rust
use rowan::{Language, SyntaxKind as RowanSyntaxKind};

// Define all possible syntax kinds for your language
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
enum SyntaxKind {
    // Tokens
    IDENT = 0,
    NUMBER,
    STRING,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    LPAREN,
    RPAREN,
    WHITESPACE,
    COMMENT,
    ERROR,
    
    // Composite nodes
    ROOT,
    EXPR,
    BINARY_EXPR,
    LITERAL,
}

// Convert between your enum and Rowan's internal representation
impl From<SyntaxKind> for RowanSyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        RowanSyntaxKind(kind as u16)
    }
}

// Define your language type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum MyLang {}

impl Language for MyLang {
    type Kind = SyntaxKind;
    
    fn kind_from_raw(raw: RowanSyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::LITERAL as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    
    fn kind_to_raw(kind: Self::Kind) -> RowanSyntaxKind {
        kind.into()
    }
}
```

## Type Aliases

Define convenient type aliases for your language:

```rust
type SyntaxNode = rowan::SyntaxNode<MyLang>;
type SyntaxToken = rowan::SyntaxToken<MyLang>;
type SyntaxElement = rowan::SyntaxElement<MyLang>;
```

## Building Your First Tree

Use `GreenNodeBuilder` to construct trees:

```rust
use rowan::GreenNodeBuilder;

fn build_simple_tree() -> SyntaxNode {
    let mut builder = GreenNodeBuilder::new();
    
    // Start the root node
    builder.start_node(SyntaxKind::ROOT.into());
    
    // Add an expression
    builder.start_node(SyntaxKind::BINARY_EXPR.into());
    
    // Left operand
    builder.start_node(SyntaxKind::LITERAL.into());
    builder.token(SyntaxKind::NUMBER.into(), "42");
    builder.finish_node();
    
    // Operator
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.token(SyntaxKind::PLUS.into(), "+");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    
    // Right operand
    builder.start_node(SyntaxKind::LITERAL.into());
    builder.token(SyntaxKind::NUMBER.into(), "13");
    builder.finish_node();
    
    builder.finish_node(); // BINARY_EXPR
    builder.finish_node(); // ROOT
    
    let green = builder.finish();
    SyntaxNode::new_root(green)
}
```

## Working with Trees

Once you have a syntax tree, you can traverse and query it:

```rust
fn explore_tree(root: &SyntaxNode) {
    println!("Root kind: {:?}", root.kind());
    println!("Root text: {:?}", root.text());
    println!("Root range: {:?}", root.text_range());
    
    // Iterate over children
    for child in root.children() {
        println!("Child: {:?}", child);
    }
    
    // Iterate over children including tokens
    for element in root.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(node) => {
                println!("Node: {:?}", node.kind());
            }
            rowan::NodeOrToken::Token(token) => {
                println!("Token: {:?} = {:?}", token.kind(), token.text());
            }
        }
    }
}
```

## Complete Example: Simple Calculator

Here's a complete example that parses and evaluates simple arithmetic expressions:

```rust
use rowan::{GreenNodeBuilder, Language, SyntaxNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
enum SyntaxKind {
    NUMBER = 0,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    LPAREN,
    RPAREN,
    WHITESPACE,
    ERROR,
    
    ROOT,
    EXPR,
    BINARY_EXPR,
    LITERAL,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        rowan::SyntaxKind(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum CalcLang {}

impl Language for CalcLang {
    type Kind = SyntaxKind;
    
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::LITERAL as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

type SyntaxNode = rowan::SyntaxNode<CalcLang>;
type SyntaxToken = rowan::SyntaxToken<CalcLang>;

// Simple lexer
fn lex(input: &str) -> Vec<(SyntaxKind, String)> {
    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();
    
    while let Some((start, ch)) = chars.next() {
        let kind = match ch {
            '+' => SyntaxKind::PLUS,
            '-' => SyntaxKind::MINUS,
            '*' => SyntaxKind::STAR,
            '/' => SyntaxKind::SLASH,
            '(' => SyntaxKind::LPAREN,
            ')' => SyntaxKind::RPAREN,
            ' ' | '\t' | '\n' | '\r' => {
                // Collect all consecutive whitespace
                let mut end = start + ch.len_utf8();
                while let Some((_, next_ch)) = chars.peek() {
                    if next_ch.is_whitespace() {
                        let (next_start, next_ch) = chars.next().unwrap();
                        end = next_start + next_ch.len_utf8();
                    } else {
                        break;
                    }
                }
                tokens.push((SyntaxKind::WHITESPACE, input[start..end].to_string()));
                continue;
            }
            '0'..='9' => {
                // Collect all consecutive digits
                let mut end = start + ch.len_utf8();
                while let Some((_, next_ch)) = chars.peek() {
                    if next_ch.is_ascii_digit() {
                        let (next_start, next_ch) = chars.next().unwrap();
                        end = next_start + next_ch.len_utf8();
                    } else {
                        break;
                    }
                }
                tokens.push((SyntaxKind::NUMBER, input[start..end].to_string()));
                continue;
            }
            _ => SyntaxKind::ERROR,
        };
        
        tokens.push((kind, ch.to_string()));
    }
    
    tokens
}

// Simple parser
fn parse(input: &str) -> SyntaxNode {
    let tokens = lex(input);
    let mut builder = GreenNodeBuilder::new();
    
    builder.start_node(SyntaxKind::ROOT.into());
    
    for (kind, text) in tokens {
        builder.token(kind.into(), &text);
    }
    
    builder.finish_node();
    
    let green = builder.finish();
    SyntaxNode::new_root(green)
}

fn main() {
    let input = "42 + 13";
    let tree = parse(input);
    
    println!("Parsed tree:");
    println!("{:#?}", tree);
    
    println!("\nTree text: {:?}", tree.text());
    println!("Tree range: {:?}", tree.text_range());
    
    // Walk the tree
    for element in tree.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(node) => {
                println!("Node: {:?} at {:?}", node.kind(), node.text_range());
            }
            rowan::NodeOrToken::Token(token) => {
                println!("Token: {:?} = {:?} at {:?}", 
                    token.kind(), token.text(), token.text_range());
            }
        }
    }
}
```

## Key Takeaways

1. **Define your language**: Create a `SyntaxKind` enum and implement the `Language` trait
2. **Use GreenNodeBuilder**: Build trees incrementally with start_node/finish_node pairs
3. **Preserve everything**: Include whitespace and comments as tokens
4. **Type safety**: Use type aliases for your specific language
5. **Immutable trees**: Trees are immutable and can be shared safely

## Next Steps

- Learn about the [Green and Red Tree Architecture](./02-architecture.md)
- Explore [Building Syntax Trees](./03-building-trees.md) in detail
- See how to integrate with [Parser Integration](./05-parser-integration.md)

The key insight is that Rowan separates the immutable tree structure (green tree) from the mutable view with parent pointers and offsets (red tree), enabling both efficiency and convenience.