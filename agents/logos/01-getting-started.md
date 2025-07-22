# Getting Started with Logos

This guide will walk you through creating your first lexer with Logos 0.15.

## Installation

Add Logos to your project using Cargo:

```bash
cargo add logos
```

Or manually add it to your `Cargo.toml`:

```toml
[dependencies]
logos = "0.15.0"
```

## Your First Lexer

Let's create a simple lexer for basic arithmetic expressions:

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Skip whitespace
enum Token {
    // Literals
    #[token("+")]
    Plus,
    
    #[token("-")]
    Minus,
    
    #[token("*")]
    Multiply,
    
    #[token("/")]
    Divide,
    
    #[token("(")]
    LeftParen,
    
    #[token(")")]
    RightParen,
    
    // Regular expressions
    #[regex("[0-9]+")]
    Number,
    
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
}

fn main() {
    let input = "x + 42 * (y - 1)";
    let mut lexer = Token::lexer(input);
    
    while let Some(token) = lexer.next() {
        match token {
            Ok(token) => println!("{:?} -> '{}'", token, lexer.slice()),
            Err(_) => println!("Error at: '{}'", lexer.slice()),
        }
    }
}
```

Output:
```
Identifier -> 'x'
Plus -> '+'
Number -> '42'
Multiply -> '*'
LeftParen -> '('
Identifier -> 'y'
Minus -> '-'
Number -> '1'
RightParen -> ')'
```

## Core Concepts

### 1. Token Enum

Your token enum represents all possible tokens in your language:

```rust
#[derive(Logos, Debug, PartialEq)]
enum Token {
    // Token variants go here
}
```

The `#[derive(Logos)]` macro generates the lexer implementation.

### 2. Token Patterns

Define patterns using attributes:

- `#[token("literal")]` - Matches exact strings
- `#[regex("pattern")]` - Matches regular expressions
- `#[logos(skip r"pattern")]` - Skips matched patterns

### 3. Lexer Usage

Create a lexer and iterate through tokens:

```rust
let mut lexer = Token::lexer("input text");

// Method 1: Iterator
for result in lexer {
    match result {
        Ok(token) => println!("Token: {:?}", token),
        Err(_) => println!("Lexing error"),
    }
}

// Method 2: Manual iteration
let mut lexer = Token::lexer("input text");
while let Some(result) = lexer.next() {
    // Handle result
}
```

## Working with Token Data

### Extracting Values

Use callbacks to extract data from matched tokens:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    Number(i32),
    
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string() // Remove quotes
    })]
    String(String),
    
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
}

fn main() {
    let mut lexer = Token::lexer(r#"42 "hello world" variable"#);
    
    while let Some(Ok(token)) = lexer.next() {
        match token {
            Token::Number(n) => println!("Number: {}", n),
            Token::String(s) => println!("String: {}", s),
            Token::Identifier => println!("Identifier: {}", lexer.slice()),
        }
    }
}
```

### Lexer Methods

The lexer provides several useful methods:

```rust
let mut lexer = Token::lexer("hello world");

// Get the current token's text
let text = lexer.slice();

// Get the current token's span (byte range)
let span = lexer.span();

// Get the remaining input
let remaining = lexer.remainder();

// Get the source text
let source = lexer.source();
```

## Error Handling

By default, Logos uses `()` as the error type. You can customize this:

```rust
#[derive(Debug, PartialEq, Clone, Default)]
enum LexError {
    #[default]
    InvalidToken,
    InvalidNumber,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>())]
    Number(i32),
    
    #[regex("[a-zA-Z]+")]
    Word,
}
```

## Common Patterns

### Skip Patterns

Skip whitespace and comments:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]                    // Skip whitespace
#[logos(skip r"//[^\n]*")]                      // Skip line comments
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]         // Skip block comments
enum Token {
    // Your tokens here
}
```

### Keywords vs Identifiers

Handle keywords that look like identifiers:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    // Keywords (higher priority)
    #[token("if")]
    If,
    
    #[token("else")]
    Else,
    
    #[token("while")]
    While,
    
    // Identifiers (lower priority)
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
}
```

Logos automatically handles priority - more specific patterns (keywords) take precedence over general ones (identifiers).

## Next Steps

Now that you understand the basics, explore:

- [Token Definition and Patterns](./02-token-definition.md) - Learn about advanced pattern matching
- [Custom Lexer Logic](./05-custom-lexer-logic.md) - Implement custom callbacks and logic
- [Error Handling](./04-error-handling.md) - Advanced error handling strategies

## Complete Example: JSON Lexer

Here's a more complete example that lexes JSON:

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
enum JsonToken {
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("null")]
    Null,

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| {
        lex.slice().parse::<f64>().unwrap()
    })]
    Number(f64),

    #[regex(r#""([^"\\]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#, |lex| {
        lex.slice().to_owned()
    })]
    String(String),
}

fn main() {
    let json = r#"{"name": "John", "age": 30, "active": true}"#;
    let mut lexer = JsonToken::lexer(json);
    
    while let Some(token) = lexer.next() {
        println!("{:?}", token);
    }
}
```

This example demonstrates:
- Boolean literals with callbacks
- Number parsing with validation
- String literals with escape sequence support
- Proper JSON syntax tokens

Continue to the next section to learn more about token definition patterns and advanced features.