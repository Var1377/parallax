# Token Definition and Patterns

This chapter covers the various ways to define tokens in Logos, from simple literals to complex regular expressions.

## Token Attributes

Logos provides three main attributes for defining tokens:

### 1. `#[token]` - Literal Strings

Use `#[token]` for exact string matches:

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[token("hello")]
    Hello,
    
    #[token("world")]
    World,
    
    #[token("==")]
    Equals,
    
    #[token("!=")]
    NotEquals,
    
    // Multi-character operators
    #[token("++")]
    Increment,
    
    #[token("--")]
    Decrement,
}
```

### 2. `#[regex]` - Regular Expressions

Use `#[regex]` for pattern matching:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    // Numbers
    #[regex("[0-9]+")]
    Integer,
    
    #[regex(r"[0-9]+\.[0-9]+")]
    Float,
    
    // Identifiers
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
    
    // Strings
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    
    // Hexadecimal numbers
    #[regex(r"0[xX][0-9a-fA-F]+")]
    HexNumber,
}
```

### 3. `#[logos]` - Global Configuration

Use `#[logos]` on the enum for global settings:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]           // Skip whitespace
#[logos(error = MyError)]              // Custom error type
#[logos(extras = MyExtras)]            // Custom extras type
enum Token {
    // Token definitions...
}
```

## Pattern Priority and Disambiguation

When multiple patterns could match the same input, Logos uses priority rules:

### Automatic Priority

Logos automatically assigns priorities based on:

1. **Longer beats shorter**: `"hello"` beats `"hell"`
2. **Specific beats generic**: `"if"` beats `[a-zA-Z]+`

```rust
#[derive(Logos, Debug, PartialEq)]
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

// "if" will match as Token::If, not Token::Identifier
```

### Manual Priority

Override automatic priority with the `priority` parameter:

```rust
#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[regex("[a-zA-Z]+", priority = 1)]
    Word,
    
    #[regex("[a-z]+", priority = 2)]  // Higher priority
    LowercaseWord,
}
```

### Priority Calculation

Logos calculates priority as follows:
- Each consecutive, non-repeating byte: +2 points
- Each character class or range: +1 point
- Loops and optional blocks: ignored
- Alternations: shortest alternative counts

Examples:
- `"foobar"` = 12 points (6 bytes × 2)
- `[a-zA-Z]+` = 2 points (1 class × 1, minimum match)
- `(foo|hello)(bar)?` = 6 points (shortest "foo" = 3 bytes × 2)

## Callbacks and Data Extraction

### Basic Callbacks

Extract data from matched tokens using callbacks:

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
    
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),
}
```

### Callback Return Types

Callbacks can return various types:

| Return Type | Behavior |
|-------------|----------|
| `T` | `Ok(Token::Variant(T))` |
| `Option<T>` | `Ok(Token::Variant(T))` or error |
| `Result<T, E>` | `Ok(Token::Variant(T))` or `Err(E)` |
| `()` | `Ok(Token::Unit)` |
| `bool` | `Ok(Token::Unit)` or error |
| `Skip` | Skip this token |
| `Filter<T>` | Emit or skip |
| `FilterResult<T, E>` | Emit, skip, or error |

### Advanced Callback Examples

```rust
use logos::{Logos, Skip, Filter, FilterResult};

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
    // Parse and validate numbers
    #[regex("[0-9]+", |lex| {
        match lex.slice().parse::<u32>() {
            Ok(n) if n <= 100 => Ok(n),
            Ok(_) => Err(LexError::InvalidNumber),
            Err(_) => Err(LexError::InvalidNumber),
        }
    })]
    SmallNumber(u32),
    
    // Skip comments
    #[regex(r"//[^\n]*", |_| Skip)]
    Comment,
    
    // Filter even numbers only
    #[regex("[0-9]+", |lex| {
        let n: u32 = lex.slice().parse().unwrap();
        if n % 2 == 0 {
            Filter::Emit(n)
        } else {
            Filter::Skip
        }
    })]
    EvenNumber(u32),
    
    // Complex filtering with errors
    #[regex("[0-9]+", |lex| {
        match lex.slice().parse::<u32>() {
            Ok(n) if n % 2 == 0 && n <= 100 => FilterResult::Emit(n),
            Ok(n) if n % 2 == 1 => FilterResult::Skip,
            Ok(_) => FilterResult::Error(LexError::InvalidNumber),
            Err(_) => FilterResult::Error(LexError::InvalidNumber),
        }
    })]
    ValidEvenNumber(u32),
}
```

## Skip Patterns

### Global Skip Patterns

Define patterns to skip globally:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]                    // Whitespace
#[logos(skip r"//[^\n]*")]                      // Line comments
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]         // Block comments
enum Token {
    #[token("fn")]
    Function,
    
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
}
```

### Token-specific Skip

Use callbacks to skip specific tokens:

```rust
#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,
    
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,
    
    #[token("important")]
    Important,
}
```

## Multiple Patterns per Token

A single token can have multiple patterns:

```rust
#[derive(Logos, Debug, PartialEq)]
enum Token {
    // Multiple literal patterns
    #[token("and")]
    #[token("&&")]
    And,
    
    #[token("or")]
    #[token("||")]
    Or,
    
    // Multiple regex patterns with different callbacks
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    #[regex("0x[0-9a-fA-F]+", |lex| {
        i32::from_str_radix(&lex.slice()[2..], 16).unwrap()
    })]
    #[regex("0b[01]+", |lex| {
        i32::from_str_radix(&lex.slice()[2..], 2).unwrap()
    })]
    Number(i32),
}
```

## Case Sensitivity

Logos patterns are case-sensitive by default. For case-insensitive matching:

```rust
#[derive(Logos, Debug, PartialEq)]
enum Token {
    // Case-sensitive (default)
    #[token("Hello")]
    Hello,
    
    // Case-insensitive using regex
    #[regex("(?i)world")]
    World,
    
    // Manual case handling
    #[token("TRUE")]
    #[token("true")]
    #[token("True")]
    True,
}
```

## Unicode Support

Logos supports Unicode in patterns:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    // Unicode identifiers
    #[regex(r"[\p{L}\p{Nl}_][\p{L}\p{Nl}\p{Nd}\p{Pc}]*")]
    Identifier,
    
    // Emoji
    #[regex(r"[\p{Emoji}]+")]
    Emoji,
    
    // Specific Unicode ranges
    #[regex(r"[α-ωΑ-Ω]+")]
    Greek,
    
    // Unicode escape sequences
    #[regex(r"\\u\{[0-9a-fA-F]+\}")]
    UnicodeEscape,
}
```

## Byte vs String Lexing

Logos can lex both strings and byte slices:

```rust
// String lexing (default)
#[derive(Logos, Debug, PartialEq)]
enum StringToken {
    #[regex("[a-zA-Z]+")]
    Word,
}

// Byte lexing
#[derive(Logos, Debug, PartialEq)]
enum ByteToken {
    #[regex(b"[a-zA-Z]+")]  // Note the 'b' prefix
    Word,
    
    #[token(b"\xFF\xFE")]   // Binary data
    BinaryMarker,
}

fn example() {
    // String lexing
    let mut string_lexer = StringToken::lexer("hello world");
    
    // Byte lexing
    let data = b"hello\xFF\xFEworld";
    let mut byte_lexer = ByteToken::lexer(data);
}
```

## Common Patterns

### Programming Language Tokens

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
enum ProgrammingToken {
    // Keywords
    #[token("fn")] Function,
    #[token("let")] Let,
    #[token("if")] If,
    #[token("else")] Else,
    #[token("while")] While,
    #[token("for")] For,
    #[token("return")] Return,
    
    // Operators
    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Multiply,
    #[token("/")] Divide,
    #[token("=")] Assign,
    #[token("==")] Equal,
    #[token("!=")] NotEqual,
    #[token("<")] Less,
    #[token(">")] Greater,
    #[token("<=")] LessEqual,
    #[token(">=")] GreaterEqual,
    
    // Delimiters
    #[token("(")] LeftParen,
    #[token(")")] RightParen,
    #[token("{")] LeftBrace,
    #[token("}")] RightBrace,
    #[token("[")] LeftBracket,
    #[token("]")] RightBracket,
    #[token(";")] Semicolon,
    #[token(",")] Comma,
    #[token(".")] Dot,
    
    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Integer(i64),
    
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),
    
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    String(String),
    
    #[regex(r"'([^'\\]|\\.)*'", |lex| {
        let s = lex.slice();
        s[1..s.len()-1].chars().next().unwrap()
    })]
    Char(char),
    
    // Identifiers (must come after keywords)
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
}
```

### JSON Tokens

```rust
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
```

## Best Practices

1. **Order Matters**: Place more specific patterns before general ones
2. **Use Callbacks**: Extract data during lexing for better performance
3. **Handle Errors**: Define custom error types for better error reporting
4. **Skip Efficiently**: Use global skip patterns for common ignored tokens
5. **Test Thoroughly**: Test edge cases and ambiguous inputs
6. **Document Patterns**: Comment complex regex patterns for maintainability

## Next Steps

- [Performance Optimization](./03-performance-optimization.md) - Learn how to optimize your lexer
- [Custom Lexer Logic](./05-custom-lexer-logic.md) - Implement advanced callback logic
- [Advanced Regex Patterns](./07-advanced-regex.md) - Master complex pattern matching