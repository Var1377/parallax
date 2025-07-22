# Custom Lexer Logic

This chapter explores advanced Logos features including callbacks, extras, context-dependent lexing, and custom source types.

## Callback Functions

### Basic Callbacks

Callbacks allow you to process matched tokens and extract data:

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    // Simple value extraction
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    Number(i32),
    
    // String processing
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string() // Remove quotes
    })]
    String(String),
    
    // Boolean literals
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),
}
```

### Advanced Callback Patterns

#### Multiple Patterns with Different Callbacks

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    #[regex("0x[0-9a-fA-F]+", |lex| {
        i32::from_str_radix(&lex.slice()[2..], 16).unwrap()
    })]
    #[regex("0b[01]+", |lex| {
        i32::from_str_radix(&lex.slice()[2..], 2).unwrap()
    })]
    #[regex("0o[0-7]+", |lex| {
        i32::from_str_radix(&lex.slice()[2..], 8).unwrap()
    })]
    Number(i32),
}

fn main() {
    let mut lexer = Token::lexer("42 0xFF 0b1010 0o755");
    
    while let Some(Ok(Token::Number(n))) = lexer.next() {
        println!("Number: {}", n);
    }
    // Output: 42, 255, 10, 493
}
```

#### Complex String Processing

```rust
use std::collections::HashMap;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex(r#""([^"\\]|\\.)*""#, process_string)]
    String(String),
    
    #[regex(r"'([^'\\]|\\.)*'", process_char)]
    Char(char),
}

fn process_string(lex: &mut logos::Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    let content = &slice[1..slice.len()-1]; // Remove quotes
    
    let mut result = String::new();
    let mut chars = content.chars();
    
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next()? {
                'n' => result.push('\n'),
                't' => result.push('\t'),
                'r' => result.push('\r'),
                '\\' => result.push('\\'),
                '"' => result.push('"'),
                'u' => {
                    // Unicode escape: \u{XXXX}
                    if chars.next()? == '{' {
                        let mut hex = String::new();
                        while let Some(c) = chars.next() {
                            if c == '}' break;
                            hex.push(c);
                        }
                        if let Ok(code) = u32::from_str_radix(&hex, 16) {
                            if let Some(unicode_char) = char::from_u32(code) {
                                result.push(unicode_char);
                            }
                        }
                    }
                }
                c => result.push(c),
            }
        } else {
            result.push(ch);
        }
    }
    
    Some(result)
}

fn process_char(lex: &mut logos::Lexer<Token>) -> Option<char> {
    let slice = lex.slice();
    let content = &slice[1..slice.len()-1]; // Remove quotes
    
    if content.len() == 1 {
        content.chars().next()
    } else if content.starts_with('\\') && content.len() == 2 {
        match content.chars().nth(1)? {
            'n' => Some('\n'),
            't' => Some('\t'),
            'r' => Some('\r'),
            '\\' => Some('\\'),
            '\'' => Some('\''),
            c => Some(c),
        }
    } else {
        None
    }
}
```

### Callback Return Types

Logos supports various callback return types:

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
    // Returns T -> Ok(Token::Value(T))
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    Number(i32),
    
    // Returns Option<T> -> Ok(Token::Value(T)) or Error
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().ok())]
    SafeNumber(i32),
    
    // Returns Result<T, E> -> Ok(Token::Value(T)) or Err(E)
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>())]
    ValidatedNumber(i32),
    
    // Returns () -> Ok(Token::Unit)
    #[regex(r"//[^\n]*", |_| ())]
    Comment,
    
    // Returns bool -> Ok(Token::Unit) or Error
    #[regex("[0-9]+", |lex| lex.slice().len() < 10)]
    ShortNumber,
    
    // Returns Skip -> skips token
    #[regex(r"[ \t]+", |_| Skip)]
    Whitespace,
    
    // Returns Filter<T> -> Emit(T) or Skip
    #[regex("[0-9]+", |lex| {
        let n: i32 = lex.slice().parse().unwrap();
        if n % 2 == 0 {
            Filter::Emit(n)
        } else {
            Filter::Skip
        }
    })]
    EvenNumber(i32),
    
    // Returns FilterResult<T, E> -> Emit(T), Skip, or Error(E)
    #[regex("[0-9]+", |lex| {
        match lex.slice().parse::<i32>() {
            Ok(n) if n > 0 => FilterResult::Emit(n),
            Ok(_) => FilterResult::Skip,
            Err(_) => FilterResult::Error(LexError::InvalidNumber),
        }
    })]
    PositiveNumber(i32),
}
```

## Extras - Lexer State

### Basic Extras Usage

Use extras to maintain state across tokens:

```rust
use logos::Logos;

#[derive(Default)]
struct LexerState {
    line: usize,
    column: usize,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = LexerState)]
#[logos(skip r"[ \t]+")]
enum Token {
    #[regex(r"\n", |lex| {
        lex.extras.line += 1;
        lex.extras.column = 0;
        (lex.extras.line, lex.extras.column)
    })]
    Newline(usize, usize),
    
    #[regex("[a-zA-Z]+", |lex| {
        let word = lex.slice().to_string();
        lex.extras.column += word.len();
        (word, lex.extras.line, lex.extras.column)
    })]
    Word(String, usize, usize),
}

fn main() {
    let input = "hello\nworld\ntest";
    let mut lexer = Token::lexer(input);
    
    while let Some(Ok(token)) = lexer.next() {
        println!("{:?}", token);
    }
}
```

### Complex State Management

```rust
use std::collections::HashMap;

#[derive(Default)]
struct ParserState {
    variables: HashMap<String, i32>,
    scope_depth: usize,
    in_string: bool,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = ParserState)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("{", |lex| {
        lex.extras.scope_depth += 1;
        lex.extras.scope_depth
    })]
    LeftBrace(usize),
    
    #[token("}", |lex| {
        lex.extras.scope_depth = lex.extras.scope_depth.saturating_sub(1);
        lex.extras.scope_depth
    })]
    RightBrace(usize),
    
    #[regex("[a-zA-Z][a-zA-Z0-9]*", |lex| {
        let name = lex.slice().to_string();
        let is_defined = lex.extras.variables.contains_key(&name);
        (name, is_defined)
    })]
    Identifier(String, bool),
    
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    Number(i32),
    
    #[token("=")]
    Assign,
}

fn main() {
    let input = "{ x = 42; y = x + 1; }";
    let mut lexer = Token::lexer(input);
    
    while let Some(Ok(token)) = lexer.next() {
        match &token {
            Token::Identifier(name, _) if lexer.slice() == "x" => {
                // Define variable x
                lexer.extras.variables.insert(name.clone(), 42);
            }
            Token::Identifier(name, _) if lexer.slice() == "y" => {
                // Define variable y
                lexer.extras.variables.insert(name.clone(), 43);
            }
            _ => {}
        }
        println!("{:?}", token);
    }
}
```

## Context-Dependent Lexing

### Lexer Morphing

Use `morph` to switch between different lexer contexts:

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
enum MainToken {
    #[token("fn")]
    Function,
    
    #[token("(")]
    LeftParen,
    
    #[token(")")]
    RightParen,
    
    #[token("{")]
    LeftBrace,
    
    #[token("\"", enter_string_mode)]
    StringStart(Vec<StringToken>),
    
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
}

#[derive(Logos, Debug, PartialEq, Clone)]
enum StringToken {
    #[token("\"")]
    StringEnd,
    
    #[regex(r"[^\"$]+")]
    Text,
    
    #[token("${", enter_interpolation)]
    InterpolationStart(Vec<MainToken>),
    
    #[token("$")]
    Dollar,
}

fn enter_string_mode(lex: &mut logos::Lexer<MainToken>) -> Option<Vec<StringToken>> {
    let mut string_lexer = lex.clone().morph::<StringToken>();
    let mut tokens = Vec::new();
    
    while let Some(token) = string_lexer.next() {
        match token {
            Ok(StringToken::StringEnd) => break,
            Ok(token) => tokens.push(token),
            Err(_) => return None,
        }
    }
    
    *lex = string_lexer.morph();
    Some(tokens)
}

fn enter_interpolation(lex: &mut logos::Lexer<StringToken>) -> Option<Vec<MainToken>> {
    let mut main_lexer = lex.clone().morph::<MainToken>();
    let mut tokens = Vec::new();
    let mut brace_count = 1;
    
    while let Some(token) = main_lexer.next() {
        match token {
            Ok(MainToken::LeftBrace) => {
                brace_count += 1;
                tokens.push(MainToken::LeftBrace);
            }
            Ok(MainToken::RightBrace) => {
                brace_count -= 1;
                if brace_count == 0 {
                    break;
                }
                tokens.push(MainToken::RightBrace);
            }
            Ok(token) => tokens.push(token),
            Err(_) => return None,
        }
    }
    
    *lex = main_lexer.morph();
    Some(tokens)
}
```

### String Interpolation Example

A complete string interpolation lexer:

```rust
use std::collections::HashMap;
use logos::Logos;

type SymbolTable = HashMap<String, String>;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"\s+")]
#[logos(extras = SymbolTable)]
enum VariableToken {
    #[regex(r"[a-zA-Z][a-zA-Z0-9]*", variable_definition)]
    Variable((String, String)),
    
    #[token("=")]
    Equals,
    
    #[token("'")]
    Quote,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = SymbolTable)]
enum StringToken {
    #[token("'")]
    Quote,
    
    #[regex(r"[^'$]+")]
    Text,
    
    #[token("${", evaluate_interpolation)]
    Interpolation(String),
    
    #[token("$")]
    Dollar,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"\s+")]
#[logos(extras = SymbolTable)]
enum InterpolationToken {
    #[regex(r"[a-zA-Z][a-zA-Z0-9]*", get_variable_value)]
    Variable(String),
    
    #[token("'")]
    Quote,
    
    #[token("}")]
    End,
}

fn variable_definition(lex: &mut logos::Lexer<VariableToken>) -> Option<(String, String)> {
    let var_name = lex.slice().to_string();
    
    // Expect '='
    if let Some(Ok(VariableToken::Equals)) = lex.next() {
        // Expect opening quote
        if let Some(Ok(VariableToken::Quote)) = lex.next() {
            // Switch to string mode
            let mut string_lexer = lex.clone().morph::<StringToken>();
            let value = get_string_content(&mut string_lexer);
            *lex = string_lexer.morph();
            
            // Store variable
            lex.extras.insert(var_name.clone(), value.clone());
            return Some((var_name, value));
        }
    }
    
    None
}

fn get_string_content(lex: &mut logos::Lexer<StringToken>) -> String {
    let mut content = String::new();
    
    while let Some(Ok(token)) = lex.next() {
        match token {
            StringToken::Text => content.push_str(lex.slice()),
            StringToken::Dollar => content.push('$'),
            StringToken::Interpolation(value) => content.push_str(&value),
            StringToken::Quote => break,
        }
    }
    
    content
}

fn evaluate_interpolation(lex: &mut logos::Lexer<StringToken>) -> Option<String> {
    let mut interp_lexer = lex.clone().morph::<InterpolationToken>();
    let mut result = String::new();
    
    while let Some(Ok(token)) = interp_lexer.next() {
        match token {
            InterpolationToken::Variable(value) => result.push_str(&value),
            InterpolationToken::Quote => {
                *lex = interp_lexer.morph();
                result.push_str(&get_string_content(lex));
                interp_lexer = lex.clone().morph();
            }
            InterpolationToken::End => break,
        }
    }
    
    *lex = interp_lexer.morph();
    Some(result)
}

fn get_variable_value(lex: &mut logos::Lexer<InterpolationToken>) -> Option<String> {
    lex.extras.get(lex.slice()).cloned()
}
```

## Custom Source Types

### Implementing Custom Sources

Create custom source types for specialized use cases:

```rust
use logos::{Logos, Source};
use std::ops::Range;

// Custom source for case-insensitive lexing
struct CaseInsensitiveSource<'a> {
    original: &'a str,
    lowercase: String,
}

impl<'a> CaseInsensitiveSource<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            original: input,
            lowercase: input.to_lowercase(),
        }
    }
}

impl<'a> Source for CaseInsensitiveSource<'a> {
    type Slice = str;
    
    fn len(&self) -> usize {
        self.lowercase.len()
    }
    
    fn read<T>(&self, offset: usize) -> Option<T>
    where
        T: logos::source::Chunk<Self::Slice>,
    {
        T::read(&self.lowercase, offset)
    }
    
    unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: logos::source::Chunk<Self::Slice>,
    {
        T::read_unchecked(&self.lowercase, offset)
    }
    
    fn slice(&self, range: Range<usize>) -> Option<&Self::Slice> {
        self.original.get(range) // Return original case
    }
    
    unsafe fn slice_unchecked(&self, range: Range<usize>) -> &Self::Slice {
        self.original.get_unchecked(range)
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(source = CaseInsensitiveSource<'s>)]
enum Token {
    #[token("hello")]
    Hello,
    
    #[token("world")]
    World,
    
    #[regex("[a-z]+")]
    Word,
}
```

### Memory-Mapped File Source

```rust
use memmap2::Mmap;
use std::fs::File;

struct MmapSource {
    mmap: Mmap,
}

impl MmapSource {
    fn new(file: File) -> std::io::Result<Self> {
        let mmap = unsafe { Mmap::map(&file)? };
        Ok(Self { mmap })
    }
}

impl Source for MmapSource {
    type Slice = [u8];
    
    fn len(&self) -> usize {
        self.mmap.len()
    }
    
    fn read<T>(&self, offset: usize) -> Option<T>
    where
        T: logos::source::Chunk<Self::Slice>,
    {
        T::read(&self.mmap, offset)
    }
    
    unsafe fn read_unchecked<T>(&self, offset: usize) -> T
    where
        T: logos::source::Chunk<Self::Slice>,
    {
        T::read_unchecked(&self.mmap, offset)
    }
    
    fn slice(&self, range: Range<usize>) -> Option<&Self::Slice> {
        self.mmap.get(range)
    }
    
    unsafe fn slice_unchecked(&self, range: Range<usize>) -> &Self::Slice {
        self.mmap.get_unchecked(range)
    }
}
```

## Advanced Techniques

### Lookahead and Context

Implement lookahead using lexer methods:

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[a-zA-Z]+", check_keyword)]
    Word(WordType),
    
    #[regex("[0-9]+")]
    Number,
}

#[derive(Debug, PartialEq)]
enum WordType {
    Keyword(String),
    Identifier(String),
    TypeName(String),
}

fn check_keyword(lex: &mut logos::Lexer<Token>) -> Option<WordType> {
    let word = lex.slice().to_string();
    
    // Check if it's a keyword
    match word.as_str() {
        "if" | "else" | "while" | "for" | "fn" | "let" => {
            return Some(WordType::Keyword(word));
        }
        _ => {}
    }
    
    // Look ahead to see if next token suggests this is a type
    let remainder = lex.remainder();
    if remainder.trim_start().starts_with(char::is_uppercase) {
        Some(WordType::TypeName(word))
    } else {
        Some(WordType::Identifier(word))
    }
}
```

### Custom Lexer Iterator

Create custom iterators for specialized token processing:

```rust
struct TokenIterator<'source> {
    lexer: logos::Lexer<'source, Token>,
    peeked: Option<Result<Token, ()>>,
}

impl<'source> TokenIterator<'source> {
    fn new(input: &'source str) -> Self {
        Self {
            lexer: Token::lexer(input),
            peeked: None,
        }
    }
    
    fn peek(&mut self) -> Option<&Result<Token, ()>> {
        if self.peeked.is_none() {
            self.peeked = self.lexer.next();
        }
        self.peeked.as_ref()
    }
    
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.next() {
            Some(Ok(token)) if token == expected => Ok(()),
            Some(Ok(token)) => Err(format!("Expected {:?}, found {:?}", expected, token)),
            Some(Err(_)) => Err("Lexer error".to_string()),
            None => Err("Unexpected end of input".to_string()),
        }
    }
}

impl<'source> Iterator for TokenIterator<'source> {
    type Item = Result<Token, ()>;
    
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.peeked.take() {
            Some(token)
        } else {
            self.lexer.next()
        }
    }
}
```

## Best Practices

1. **Keep Callbacks Simple**: Complex logic should be in separate functions
2. **Use Extras Judiciously**: Only store necessary state to avoid performance overhead
3. **Handle Errors Gracefully**: Always consider error cases in callbacks
4. **Test Context Switching**: Thoroughly test lexer morphing scenarios
5. **Document Complex Logic**: Comment complex callback and state management code
6. **Profile Performance**: Measure the impact of custom logic on lexer performance

## Next Steps

- [Parser Integration](./06-parser-integration.md) - Learn how to integrate with parser generators
- [Advanced Regex Patterns](./07-advanced-regex.md) - Master complex pattern matching
- [API Reference](./09-api-reference.md) - Complete API documentation