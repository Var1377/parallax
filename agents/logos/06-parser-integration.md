# Parser Integration

This chapter covers how to integrate Logos lexers with various parser generators and parsing libraries, including Chumsky, Nom, Pest, and custom recursive descent parsers.

## Integration Patterns

### Token-Based Parsing

Most parser generators work with token streams rather than raw text:

```rust
// 1. Define tokens with Logos
// 2. Lex input into tokens
// 3. Parse tokens with parser generator
// 4. Handle errors from both lexer and parser
```

### Common Integration Workflow

```rust
use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("(")]
    LeftParen,
    
    #[token(")")]
    RightParen,
    
    #[token("+")]
    Plus,
    
    #[token("*")]
    Multiply,
    
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    Number(i32),
}

// Generic tokenization function
fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut lexer = Token::lexer(input);
    
    while let Some(result) = lexer.next() {
        match result {
            Ok(token) => tokens.push(token),
            Err(_) => return Err(format!("Lexer error at position {}", lexer.span().start)),
        }
    }
    
    Ok(tokens)
}
```

## Chumsky Integration

Chumsky is a parser combinator library that works well with Logos:

```toml
[dependencies]
logos = "0.15"
chumsky = "0.10"
```

### Basic Chumsky Parser

```rust
use logos::Logos;
use chumsky::prelude::*;

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
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
    
    #[regex("[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Number(i64),
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

fn parser<'src>() -> impl Parser<'src, &'src [Token], Expr, extra::Err<Simple<'src, Token>>> {
    recursive(|expr| {
        let number = select! {
            Token::Number(n) => Expr::Number(n),
        };
        
        let atom = number.or(expr.clone().delimited_by(
            just(Token::LeftParen),
            just(Token::RightParen)
        ));
        
        let factor = atom;
        
        let term = factor.clone().foldl(
            choice((
                just(Token::Multiply).to(BinaryOp::Mul),
                just(Token::Divide).to(BinaryOp::Div),
            ))
            .then(factor)
            .repeated(),
            |left, (op, right)| Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
        );
        
        let expr = term.clone().foldl(
            choice((
                just(Token::Plus).to(BinaryOp::Add),
                just(Token::Minus).to(BinaryOp::Sub),
            ))
            .then(term)
            .repeated(),
            |left, (op, right)| Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
        );
        
        expr
    })
}

fn main() {
    let input = "1 + 2 * (3 - 4)";
    
    // Tokenize
    let tokens: Vec<_> = Token::lexer(input)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    
    // Parse
    let ast = parser().parse(&tokens).into_result().unwrap();
    println!("{:#?}", ast);
}
```

### Advanced Chumsky Integration

```rust
use logos::{Logos, Span};
use chumsky::prelude::*;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("fn")]
    Function,
    
    #[token("let")]
    Let,
    
    #[token("if")]
    If,
    
    #[token("else")]
    Else,
    
    #[token("=")]
    Assign,
    
    #[token("==")]
    Equal,
    
    #[token("(")]
    LeftParen,
    
    #[token(")")]
    RightParen,
    
    #[token("{")]
    LeftBrace,
    
    #[token("}")]
    RightBrace,
    
    #[token(";")]
    Semicolon,
    
    #[regex("[a-zA-Z][a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    Identifier(String),
    
    #[regex("[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Number(i64),
}

// Spanned tokens for better error reporting
type Spanned<T> = (T, Span);

fn lexer_with_spans(input: &str) -> Vec<Spanned<Token>> {
    Token::lexer(input)
        .spanned()
        .filter_map(|(token, span)| token.ok().map(|t| (t, span)))
        .collect()
}

#[derive(Debug)]
enum Statement {
    Let {
        name: String,
        value: Expr,
    },
    Expression(Expr),
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    Identifier(String),
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Equal,
}

fn statement_parser<'src>() -> impl Parser<'src, &'src [Spanned<Token>], Statement, extra::Err<Simple<'src, Spanned<Token>>>> {
    let expr = recursive(|expr| {
        let number = select! {
            (Token::Number(n), _) => Expr::Number(n),
        };
        
        let ident = select! {
            (Token::Identifier(name), _) => Expr::Identifier(name),
        };
        
        let atom = number.or(ident);
        
        let binary = atom.clone().foldl(
            choice((
                just((Token::Equal, 0..0)).to(BinaryOp::Equal),
            ))
            .then(atom)
            .repeated(),
            |left, (op, right)| Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
        );
        
        binary
    });
    
    let let_stmt = just((Token::Let, 0..0))
        .ignore_then(select! {
            (Token::Identifier(name), _) => name,
        })
        .then_ignore(just((Token::Assign, 0..0)))
        .then(expr.clone())
        .then_ignore(just((Token::Semicolon, 0..0)))
        .map(|(name, value)| Statement::Let { name, value });
    
    let expr_stmt = expr
        .then_ignore(just((Token::Semicolon, 0..0)))
        .map(Statement::Expression);
    
    choice((let_stmt, expr_stmt))
}
```

## Nom Integration

Nom is a parser combinator library that can work with token streams:

```toml
[dependencies]
logos = "0.15"
nom = "7.1"
```

### Basic Nom Parser

```rust
use logos::Logos;
use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("(")]
    LeftParen,
    
    #[token(")")]
    RightParen,
    
    #[token("+")]
    Plus,
    
    #[token("-")]
    Minus,
    
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    Number(i32),
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
}

// Custom nom combinator for tokens
fn token(expected: Token) -> impl Fn(&[Token]) -> IResult<&[Token], Token> {
    move |input: &[Token]| {
        if let Some((first, rest)) = input.split_first() {
            if *first == expected {
                Ok((rest, first.clone()))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        } else {
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Eof,
            )))
        }
    }
}

fn number(input: &[Token]) -> IResult<&[Token], Expr> {
    map(
        |input: &[Token]| {
            if let Some((Token::Number(n), rest)) = input.split_first() {
                Ok((rest, *n))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        },
        Expr::Number,
    )(input)
}

fn factor(input: &[Token]) -> IResult<&[Token], Expr> {
    alt((
        number,
        delimited(token(Token::LeftParen), expression, token(Token::RightParen)),
    ))(input)
}

fn expression(input: &[Token]) -> IResult<&[Token], Expr> {
    let (input, first) = factor(input)?;
    
    many0(pair(
        alt((token(Token::Plus), token(Token::Minus))),
        factor,
    ))(input)
    .map(|(input, ops)| {
        (
            input,
            ops.into_iter().fold(first, |acc, (op, expr)| match op {
                Token::Plus => Expr::Add(Box::new(acc), Box::new(expr)),
                Token::Minus => Expr::Sub(Box::new(acc), Box::new(expr)),
                _ => unreachable!(),
            }),
        )
    })
}

fn main() {
    let input = "1 + 2 - 3";
    let tokens: Vec<_> = Token::lexer(input)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    
    let (_, ast) = expression(&tokens).unwrap();
    println!("{:#?}", ast);
}
```

## Custom Recursive Descent Parser

Sometimes a hand-written parser is the best choice:

```rust
use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("fn")]
    Function,
    
    #[token("let")]
    Let,
    
    #[token("if")]
    If,
    
    #[token("else")]
    Else,
    
    #[token("while")]
    While,
    
    #[token("return")]
    Return,
    
    #[token("=")]
    Assign,
    
    #[token("==")]
    Equal,
    
    #[token("!=")]
    NotEqual,
    
    #[token("<")]
    Less,
    
    #[token(">")]
    Greater,
    
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
    
    #[token("{")]
    LeftBrace,
    
    #[token("}")]
    RightBrace,
    
    #[token(";")]
    Semicolon,
    
    #[regex("[a-zA-Z][a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    Identifier(String),
    
    #[regex("[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Number(i64),
}

#[derive(Debug)]
enum Statement {
    Let { name: String, value: Expr },
    If { condition: Expr, then_block: Vec<Statement>, else_block: Option<Vec<Statement>> },
    While { condition: Expr, body: Vec<Statement> },
    Return(Option<Expr>),
    Expression(Expr),
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    Identifier(String),
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
    Call { name: String, args: Vec<Expr> },
}

#[derive(Debug)]
enum BinaryOp {
    Add, Sub, Mul, Div, Equal, NotEqual, Less, Greater,
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
    
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }
    
    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens.get(self.current - 1)
    }
    
    fn check(&self, token: &Token) -> bool {
        if let Some(current) = self.peek() {
            std::mem::discriminant(current) == std::mem::discriminant(token)
        } else {
            false
        }
    }
    
    fn consume(&mut self, expected: Token) -> Result<(), String> {
        if self.check(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, found {:?}", expected, self.peek()))
        }
    }
    
    fn parse_program(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        
        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        
        Ok(statements)
    }
    
    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Some(Token::Let) => self.parse_let(),
            Some(Token::If) => self.parse_if(),
            Some(Token::While) => self.parse_while(),
            Some(Token::Return) => self.parse_return(),
            _ => {
                let expr = self.parse_expression()?;
                self.consume(Token::Semicolon)?;
                Ok(Statement::Expression(expr))
            }
        }
    }
    
    fn parse_let(&mut self) -> Result<Statement, String> {
        self.consume(Token::Let)?;
        
        let name = match self.advance() {
            Some(Token::Identifier(name)) => name.clone(),
            _ => return Err("Expected identifier after 'let'".to_string()),
        };
        
        self.consume(Token::Assign)?;
        let value = self.parse_expression()?;
        self.consume(Token::Semicolon)?;
        
        Ok(Statement::Let { name, value })
    }
    
    fn parse_if(&mut self) -> Result<Statement, String> {
        self.consume(Token::If)?;
        self.consume(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume(Token::RightParen)?;
        
        let then_block = self.parse_block()?;
        
        let else_block = if self.check(&Token::Else) {
            self.advance();
            Some(self.parse_block()?)
        } else {
            None
        };
        
        Ok(Statement::If { condition, then_block, else_block })
    }
    
    fn parse_while(&mut self) -> Result<Statement, String> {
        self.consume(Token::While)?;
        self.consume(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume(Token::RightParen)?;
        let body = self.parse_block()?;
        
        Ok(Statement::While { condition, body })
    }
    
    fn parse_return(&mut self) -> Result<Statement, String> {
        self.consume(Token::Return)?;
        
        let value = if self.check(&Token::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        
        self.consume(Token::Semicolon)?;
        Ok(Statement::Return(value))
    }
    
    fn parse_block(&mut self) -> Result<Vec<Statement>, String> {
        self.consume(Token::LeftBrace)?;
        let mut statements = Vec::new();
        
        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        
        self.consume(Token::RightBrace)?;
        Ok(statements)
    }
    
    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_equality()
    }
    
    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_comparison()?;
        
        while let Some(token) = self.peek() {
            let op = match token {
                Token::Equal => BinaryOp::Equal,
                Token::NotEqual => BinaryOp::NotEqual,
                _ => break,
            };
            
            self.advance();
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_term()?;
        
        while let Some(token) = self.peek() {
            let op = match token {
                Token::Less => BinaryOp::Less,
                Token::Greater => BinaryOp::Greater,
                _ => break,
            };
            
            self.advance();
            let right = self.parse_term()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_factor()?;
        
        while let Some(token) = self.peek() {
            let op = match token {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => break,
            };
            
            self.advance();
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;
        
        while let Some(token) = self.peek() {
            let op = match token {
                Token::Multiply => BinaryOp::Mul,
                Token::Divide => BinaryOp::Div,
                _ => break,
            };
            
            self.advance();
            let right = self.parse_primary()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }
    
    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.advance() {
            Some(Token::Number(n)) => Ok(Expr::Number(*n)),
            Some(Token::Identifier(name)) => {
                if self.check(&Token::LeftParen) {
                    // Function call
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    
                    if !self.check(&Token::RightParen) {
                        loop {
                            args.push(self.parse_expression()?);
                            if !self.check(&Token::Semicolon) {
                                break;
                            }
                            self.advance(); // consume ','
                        }
                    }
                    
                    self.consume(Token::RightParen)?;
                    Ok(Expr::Call { name: name.clone(), args })
                } else {
                    Ok(Expr::Identifier(name.clone()))
                }
            }
            Some(Token::LeftParen) => {
                let expr = self.parse_expression()?;
                self.consume(Token::RightParen)?;
                Ok(expr)
            }
            _ => Err("Expected expression".to_string()),
        }
    }
}

fn main() {
    let input = r#"
        let x = 42;
        if (x > 0) {
            return x + 1;
        } else {
            return 0;
        }
    "#;
    
    let tokens: Vec<_> = Token::lexer(input)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program().unwrap();
    
    println!("{:#?}", ast);
}
```

## Error Handling in Parser Integration

### Comprehensive Error Handling

```rust
use logos::{Logos, Span};

#[derive(Debug)]
enum ParseError {
    LexError { position: usize, message: String },
    ParseError { token: Token, expected: String },
    UnexpectedEof,
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    Number(i32),
    
    #[token("+")]
    Plus,
    
    #[token("(")]
    LeftParen,
    
    #[token(")")]
    RightParen,
}

fn tokenize_with_errors(input: &str) -> Result<Vec<(Token, Span)>, ParseError> {
    let mut tokens = Vec::new();
    let mut lexer = Token::lexer(input);
    
    while let Some(result) = lexer.next() {
        match result {
            Ok(token) => tokens.push((token, lexer.span())),
            Err(_) => {
                return Err(ParseError::LexError {
                    position: lexer.span().start,
                    message: format!("Invalid token: '{}'", lexer.slice()),
                });
            }
        }
    }
    
    Ok(tokens)
}

fn parse_with_recovery(input: &str) -> Result<Expr, Vec<ParseError>> {
    let tokens = match tokenize_with_errors(input) {
        Ok(tokens) => tokens,
        Err(err) => return Err(vec![err]),
    };
    
    // Parse with error recovery
    // Implementation would collect multiple errors
    // and attempt to continue parsing
    
    todo!("Implement error recovery parser")
}
```

## Best Practices

### 1. Token Design for Parsing

Design tokens that make parsing easier:

```rust
// ✅ Good - specific tokens
#[derive(Logos, Debug, Clone, PartialEq)]
enum Token {
    #[token("==")]
    Equal,
    
    #[token("!=")]
    NotEqual,
    
    #[token("<=")]
    LessEqual,
    
    #[token(">=")]
    GreaterEqual,
}

// ❌ Poor - ambiguous tokens
enum BadToken {
    #[token("=")]
    Assign,
    
    #[token("=")]  // Conflict!
    Equal,
}
```

### 2. Error Recovery

Implement error recovery in both lexer and parser:

```rust
fn parse_with_recovery(input: &str) -> (Option<Ast>, Vec<Error>) {
    let mut errors = Vec::new();
    
    // Collect lexer errors
    let tokens = tokenize_with_error_collection(input, &mut errors);
    
    // Parse with error recovery
    let ast = parse_with_error_recovery(&tokens, &mut errors);
    
    (ast, errors)
}
```

### 3. Span Information

Preserve span information for better error reporting:

```rust
#[derive(Debug)]
struct SpannedExpr {
    expr: Expr,
    span: Span,
}

fn parse_with_spans(tokens: &[(Token, Span)]) -> SpannedExpr {
    // Implementation preserves span information
    todo!()
}
```

## Testing Parser Integration

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_simple_expression() {
        let input = "1 + 2 * 3";
        let tokens = tokenize(input).unwrap();
        let ast = parse(&tokens).unwrap();
        
        // Verify AST structure
        assert!(matches!(ast, Expr::Binary { .. }));
    }
    
    #[test]
    fn test_error_handling() {
        let input = "1 + @";
        let result = parse_with_recovery(input);
        
        assert!(result.1.len() > 0); // Should have errors
    }
    
    #[test]
    fn test_complex_program() {
        let input = r#"
            fn main() {
                let x = 42;
                if (x > 0) {
                    return x;
                }
            }
        "#;
        
        let tokens = tokenize(input).unwrap();
        let ast = parse_program(&tokens).unwrap();
        
        // Verify program structure
        assert_eq!(ast.len(), 1); // One function
    }
}
```

## Next Steps

- [Advanced Regex Patterns](./07-advanced-regex.md) - Learn complex pattern matching
- [Benchmarking and Profiling](./08-benchmarking.md) - Measure parser performance
- [API Reference](./09-api-reference.md) - Complete API documentation