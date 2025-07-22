# Error Handling and Recovery

Robust error handling is crucial for creating reliable lexers. This chapter covers Logos' error handling mechanisms, custom error types, and recovery strategies.

## Default Error Handling

### Basic Error Behavior

By default, Logos uses `()` as the error type:

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("hello")]
    Hello,
    
    #[regex("[a-zA-Z]+")]
    Word,
    
    #[regex("[0-9]+")]
    Number,
}

fn main() {
    let mut lexer = Token::lexer("hello 123 @#$");
    
    while let Some(result) = lexer.next() {
        match result {
            Ok(token) => println!("Token: {:?}", token),
            Err(_) => println!("Error at: '{}'", lexer.slice()),
        }
    }
}
```

Output:
```
Token: Hello
Token: Number
Error at: '@'
Error at: '#'
Error at: '$'
```

## Custom Error Types

### Defining Custom Errors

Create custom error types for better error reporting:

```rust
use logos::Logos;

#[derive(Debug, PartialEq, Clone, Default)]
enum LexError {
    #[default]
    InvalidToken,
    InvalidNumber,
    InvalidString,
    UnterminatedString,
    InvalidEscape,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
    
    #[regex("[0-9]+", |lex| lex.slice().parse::<u32>())]
    Number(u32),
    
    #[regex(r#""([^"\\]|\\.)*""#, parse_string)]
    String(String),
}

fn parse_string(lex: &mut logos::Lexer<Token>) -> Result<String, LexError> {
    let slice = lex.slice();
    if !slice.ends_with('"') {
        return Err(LexError::UnterminatedString);
    }
    
    let content = &slice[1..slice.len()-1];
    let mut result = String::new();
    let mut chars = content.chars();
    
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(_) => return Err(LexError::InvalidEscape),
                None => return Err(LexError::InvalidEscape),
            }
        } else {
            result.push(ch);
        }
    }
    
    Ok(result)
}
```

### Error Conversion

Implement `From` traits for automatic error conversion:

```rust
use std::num::ParseIntError;

#[derive(Debug, PartialEq, Clone, Default)]
enum LexError {
    #[default]
    InvalidToken,
    NumberParseError(String),
    Overflow,
}

impl From<ParseIntError> for LexError {
    fn from(err: ParseIntError) -> Self {
        use std::num::IntErrorKind::*;
        match err.kind() {
            PosOverflow | NegOverflow => LexError::Overflow,
            _ => LexError::NumberParseError(err.to_string()),
        }
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
enum Token {
    #[regex("[0-9]+", |lex| lex.slice().parse::<u32>())]
    Number(u32),
}
```

## Advanced Error Handling

### Error Callbacks

Use custom error callbacks for context-specific error handling:

```rust
#[derive(Debug, PartialEq, Clone, Default)]
enum LexError {
    #[default]
    InvalidToken,
    NonAsciiCharacter(char),
    InvalidNumber(String),
}

impl LexError {
    fn from_lexer(lex: &mut logos::Lexer<Token>) -> Self {
        let slice = lex.slice();
        if let Some(ch) = slice.chars().next() {
            if !ch.is_ascii() {
                return LexError::NonAsciiCharacter(ch);
            }
        }
        LexError::InvalidToken
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error(LexError, LexError::from_lexer))]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[a-zA-Z]+")]
    Word,
    
    #[regex("[0-9]+", |lex| lex.slice().parse::<u32>())]
    Number(u32),
}

fn main() {
    let mut lexer = Token::lexer("hello 123 café");
    
    while let Some(result) = lexer.next() {
        match result {
            Ok(token) => println!("Token: {:?}", token),
            Err(LexError::NonAsciiCharacter(ch)) => {
                println!("Non-ASCII character: '{}' at position {}", ch, lexer.span().start);
            }
            Err(err) => println!("Error: {:?}", err),
        }
    }
}
```

### Conditional Error Handling

Use `FilterResult` for complex error conditions:

```rust
use logos::{Logos, FilterResult};

#[derive(Debug, PartialEq, Clone, Default)]
enum LexError {
    #[default]
    InvalidToken,
    NumberTooLarge,
    NumberTooSmall,
    InvalidRange,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[0-9]+", validate_number)]
    Number(u32),
    
    #[regex("[a-zA-Z]+")]
    Word,
}

fn validate_number(lex: &mut logos::Lexer<Token>) -> FilterResult<u32, LexError> {
    match lex.slice().parse::<u32>() {
        Ok(n) if n > 1000 => FilterResult::Error(LexError::NumberTooLarge),
        Ok(n) if n < 10 => FilterResult::Error(LexError::NumberTooSmall),
        Ok(n) => FilterResult::Emit(n),
        Err(_) => FilterResult::Error(LexError::InvalidRange),
    }
}
```

## Error Recovery Strategies

### Skip and Continue

Implement error recovery by skipping invalid tokens:

```rust
use logos::Logos;

#[derive(Debug, PartialEq, Clone, Default)]
enum LexError {
    #[default]
    InvalidToken,
    RecoveredError(String),
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[a-zA-Z]+")]
    Word,
    
    #[regex("[0-9]+")]
    Number,
    
    #[token(";")]
    Semicolon,
}

fn lex_with_recovery(input: &str) -> Vec<Result<Token, LexError>> {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();
    
    while lexer.remainder().len() > 0 {
        match lexer.next() {
            Some(Ok(token)) => tokens.push(Ok(token)),
            Some(Err(err)) => {
                // Record the error
                let error_text = lexer.slice().to_string();
                tokens.push(Err(LexError::RecoveredError(error_text)));
                
                // Skip the problematic character and continue
                lexer.bump(1);
            }
            None => break,
        }
    }
    
    tokens
}

fn main() {
    let input = "hello 123 @#$ world 456;";
    let tokens = lex_with_recovery(input);
    
    for token in tokens {
        match token {
            Ok(t) => println!("Token: {:?}", t),
            Err(e) => println!("Recovered from error: {:?}", e),
        }
    }
}
```

### Synchronization Points

Use synchronization points to recover from errors:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("fn")]
    Function,
    
    #[token("{")]
    LeftBrace,
    
    #[token("}")]
    RightBrace,
    
    #[token(";")]
    Semicolon,
    
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
    
    #[regex("[0-9]+")]
    Number,
}

fn lex_with_sync(input: &str) -> Vec<Token> {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();
    
    while let Some(result) = lexer.next() {
        match result {
            Ok(token) => tokens.push(token),
            Err(_) => {
                // Skip until we find a synchronization point
                while let Some(result) = lexer.next() {
                    if let Ok(token) = result {
                        match token {
                            Token::Semicolon | Token::RightBrace => {
                                tokens.push(token);
                                break;
                            }
                            _ => tokens.push(token),
                        }
                    }
                }
            }
        }
    }
    
    tokens
}
```

## Error Reporting

### Rich Error Messages

Create detailed error messages with context:

```rust
use logos::{Logos, Span};

#[derive(Debug, Clone)]
struct LexError {
    kind: ErrorKind,
    span: Span,
    context: String,
}

#[derive(Debug, Clone)]
enum ErrorKind {
    InvalidToken,
    UnterminatedString,
    InvalidNumber,
    UnexpectedCharacter(char),
}

impl Default for LexError {
    fn default() -> Self {
        LexError {
            kind: ErrorKind::InvalidToken,
            span: 0..0,
            context: String::new(),
        }
    }
}

impl PartialEq for LexError {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(&self.kind) == std::mem::discriminant(&other.kind)
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[a-zA-Z]+")]
    Word,
    
    #[regex("[0-9]+")]
    Number,
    
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
}

fn lex_with_context(input: &str) -> Vec<Result<Token, LexError>> {
    let mut lexer = Token::lexer(input);
    let mut results = Vec::new();
    
    while let Some(result) = lexer.next() {
        match result {
            Ok(token) => results.push(Ok(token)),
            Err(_) => {
                let span = lexer.span();
                let context = get_line_context(input, span.start);
                let error = LexError {
                    kind: ErrorKind::InvalidToken,
                    span,
                    context,
                };
                results.push(Err(error));
            }
        }
    }
    
    results
}

fn get_line_context(input: &str, position: usize) -> String {
    let lines: Vec<&str> = input.lines().collect();
    let mut current_pos = 0;
    
    for (line_num, line) in lines.iter().enumerate() {
        let line_end = current_pos + line.len();
        if position <= line_end {
            let column = position - current_pos;
            return format!("Line {}, Column {}: {}", line_num + 1, column + 1, line);
        }
        current_pos = line_end + 1; // +1 for newline
    }
    
    "Unknown location".to_string()
}
```

### Integration with Error Reporting Crates

Use `ariadne` for beautiful error reports:

```toml
[dependencies]
logos = "0.15"
ariadne = "0.4"
```

```rust
use logos::Logos;
use ariadne::{Report, ReportKind, Label, Source, Color};

#[derive(Debug, PartialEq, Clone, Default)]
enum LexError {
    #[default]
    InvalidToken,
    UnterminatedString,
    InvalidNumber,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[a-zA-Z]+")]
    Word,
    
    #[regex("[0-9]+")]
    Number,
    
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
}

fn report_errors(filename: &str, input: &str) {
    let mut lexer = Token::lexer(input);
    
    while let Some(result) = lexer.next() {
        if let Err(error) = result {
            let span = lexer.span();
            
            Report::build(ReportKind::Error, filename, span.start)
                .with_message("Lexical error")
                .with_label(
                    Label::new((filename, span))
                        .with_message(format!("{:?}", error))
                        .with_color(Color::Red)
                )
                .finish()
                .eprint((filename, Source::from(input)))
                .unwrap();
        }
    }
}

fn main() {
    let input = r#"hello "unterminated string world 123"#;
    report_errors("example.txt", input);
}
```

## Testing Error Handling

### Unit Tests for Errors

Write comprehensive tests for error conditions:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_invalid_token() {
        let mut lexer = Token::lexer("@#$");
        assert_eq!(lexer.next(), Some(Err(LexError::InvalidToken)));
    }
    
    #[test]
    fn test_unterminated_string() {
        let mut lexer = Token::lexer(r#""hello world"#);
        assert_eq!(lexer.next(), Some(Err(LexError::UnterminatedString)));
    }
    
    #[test]
    fn test_number_overflow() {
        let mut lexer = Token::lexer("999999999999999999999");
        match lexer.next() {
            Some(Err(LexError::NumberParseError(_))) => (),
            other => panic!("Expected number parse error, got {:?}", other),
        }
    }
    
    #[test]
    fn test_error_recovery() {
        let tokens = lex_with_recovery("hello @#$ world");
        assert_eq!(tokens.len(), 4); // hello, @, #, $, world (with errors)
    }
}
```

### Property-based Testing

Use `proptest` for comprehensive error testing:

```toml
[dev-dependencies]
proptest = "1.0"
```

```rust
#[cfg(test)]
mod prop_tests {
    use super::*;
    use proptest::prelude::*;
    
    proptest! {
        #[test]
        fn lexer_never_panics(input in ".*") {
            let mut lexer = Token::lexer(&input);
            while let Some(_) = lexer.next() {
                // Just ensure we don't panic
            }
        }
        
        #[test]
        fn valid_tokens_never_error(
            words in prop::collection::vec("[a-zA-Z]+", 0..100)
        ) {
            let input = words.join(" ");
            let mut lexer = Token::lexer(&input);
            
            while let Some(result) = lexer.next() {
                assert!(result.is_ok(), "Valid input should not produce errors");
            }
        }
    }
}
```

## Best Practices

### 1. Design Error Types Carefully

```rust
// ✅ Good - specific error types
#[derive(Debug, PartialEq, Clone)]
enum LexError {
    InvalidToken { position: usize, character: char },
    UnterminatedString { start: usize },
    NumberOverflow { value: String },
    InvalidEscape { sequence: String },
}

// ❌ Poor - generic error types
#[derive(Debug, PartialEq, Clone)]
enum LexError {
    Error(String),
}
```

### 2. Provide Context

Always include position information and context in errors:

```rust
#[derive(Debug, Clone)]
struct LexError {
    kind: ErrorKind,
    span: Span,
    line: usize,
    column: usize,
    context: String,
}
```

### 3. Implement Recovery

Design your lexer to recover from errors when possible:

```rust
fn recover_from_error(lexer: &mut Lexer<Token>) {
    // Skip to next whitespace or delimiter
    while let Some(ch) = lexer.remainder().chars().next() {
        if ch.is_whitespace() || "(){}[];,".contains(ch) {
            break;
        }
        lexer.bump(ch.len_utf8());
    }
}
```

### 4. Test Error Paths

Ensure all error paths are tested:

```rust
#[test]
fn test_all_error_types() {
    let test_cases = vec![
        ("@", LexError::InvalidToken),
        ("\"unterminated", LexError::UnterminatedString),
        ("999999999999999999999", LexError::NumberOverflow),
        ("\"\\z\"", LexError::InvalidEscape),
    ];
    
    for (input, expected_error) in test_cases {
        let mut lexer = Token::lexer(input);
        match lexer.next() {
            Some(Err(err)) => assert_eq!(err, expected_error),
            other => panic!("Expected error {:?}, got {:?}", expected_error, other),
        }
    }
}
```

## Next Steps

- [Custom Lexer Logic](./05-custom-lexer-logic.md) - Learn about advanced callback techniques
- [Parser Integration](./06-parser-integration.md) - Integrate error handling with parsers
- [API Reference](./09-api-reference.md) - Complete error handling API reference