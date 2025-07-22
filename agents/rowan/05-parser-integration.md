# Parser Integration

This guide covers how to integrate Rowan with different parsing approaches, from hand-written recursive descent parsers to parser generators. Rowan's design makes it particularly well-suited for error-recovering parsers that need to preserve all source text.

## Overview

Rowan integrates with parsers through the `GreenNodeBuilder` API. The key principles are:

- **Lossless parsing**: Preserve all source text including whitespace and comments
- **Error recovery**: Continue parsing after errors and include error nodes
- **Incremental building**: Build trees as you parse, not after
- **Structural preservation**: Maintain the hierarchical structure of the source

## Basic Parser Integration Pattern

### Parser Structure

```rust
use rowan::{GreenNodeBuilder, GreenNode, Language};

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    builder: GreenNodeBuilder<'a>,
    errors: Vec<ParseError>,
}

#[derive(Debug, Clone)]
struct Token {
    kind: SyntaxKind,
    text: String,
    range: TextRange,
}

#[derive(Debug)]
struct ParseError {
    message: String,
    range: TextRange,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            pos: 0,
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }
    
    fn parse(mut self) -> ParseResult {
        self.builder.start_node(SyntaxKind::ROOT.into());
        
        while !self.at_end() {
            self.parse_item();
        }
        
        self.builder.finish_node();
        
        ParseResult {
            green: self.builder.finish(),
            errors: self.errors,
        }
    }
}

struct ParseResult {
    green: GreenNode,
    errors: Vec<ParseError>,
}
```

### Token Consumption

```rust
impl<'a> Parser<'a> {
    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
    
    fn at(&self, kind: SyntaxKind) -> bool {
        self.current().map(|t| t.kind) == Some(kind)
    }
    
    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }
    
    fn advance(&mut self) {
        if let Some(token) = self.current() {
            self.builder.token(token.kind.into(), &token.text);
            self.pos += 1;
        }
    }
    
    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            self.error(format!("expected {:?}", kind));
            false
        }
    }
    
    fn error(&mut self, message: String) {
        if let Some(token) = self.current() {
            self.errors.push(ParseError {
                message,
                range: token.range,
            });
        }
    }
}
```

## Recursive Descent Parser

### Expression Parsing with Precedence

```rust
impl<'a> Parser<'a> {
    fn parse_expression(&mut self) {
        self.parse_expression_with_precedence(0);
    }
    
    fn parse_expression_with_precedence(&mut self, min_precedence: u8) {
        let checkpoint = self.builder.checkpoint();
        self.parse_primary_expression();
        
        while let Some(op) = self.current_binary_operator() {
            let precedence = op.precedence();
            if precedence < min_precedence {
                break;
            }
            
            // Wrap the left operand in a binary expression
            self.builder.start_node_at(checkpoint, SyntaxKind::BINARY_EXPR.into());
            
            // Consume the operator
            self.advance();
            
            // Parse the right operand with higher precedence
            let next_min_precedence = if op.is_left_associative() {
                precedence + 1
            } else {
                precedence
            };
            self.parse_expression_with_precedence(next_min_precedence);
            
            self.builder.finish_node();
        }
    }
    
    fn parse_primary_expression(&mut self) {
        match self.current().map(|t| t.kind) {
            Some(SyntaxKind::NUMBER) => {
                self.builder.start_node(SyntaxKind::LITERAL.into());
                self.advance();
                self.builder.finish_node();
            }
            Some(SyntaxKind::IDENT) => {
                self.builder.start_node(SyntaxKind::PATH_EXPR.into());
                self.advance();
                self.builder.finish_node();
            }
            Some(SyntaxKind::L_PAREN) => {
                self.builder.start_node(SyntaxKind::PAREN_EXPR.into());
                self.advance(); // '('
                self.parse_expression();
                self.expect(SyntaxKind::R_PAREN);
                self.builder.finish_node();
            }
            _ => {
                self.error("expected expression".to_string());
                self.recover_to_expression_boundary();
            }
        }
    }
}
```

### Statement Parsing

```rust
impl<'a> Parser<'a> {
    fn parse_item(&mut self) {
        // Skip leading whitespace and comments
        self.skip_trivia();
        
        if self.at_end() {
            return;
        }
        
        match self.current().unwrap().kind {
            SyntaxKind::FN_KW => self.parse_function(),
            SyntaxKind::LET_KW => self.parse_let_statement(),
            SyntaxKind::IF_KW => self.parse_if_statement(),
            _ => {
                self.error("expected item".to_string());
                self.recover_to_item_boundary();
            }
        }
    }
    
    fn parse_function(&mut self) {
        self.builder.start_node(SyntaxKind::FUNCTION.into());
        
        self.expect(SyntaxKind::FN_KW);
        self.skip_trivia();
        
        if !self.expect(SyntaxKind::IDENT) {
            self.recover_in_function();
        }
        
        self.skip_trivia();
        self.parse_parameter_list();
        self.skip_trivia();
        
        if self.at(SyntaxKind::ARROW) {
            self.advance(); // '->'
            self.skip_trivia();
            self.parse_type();
            self.skip_trivia();
        }
        
        self.parse_block();
        
        self.builder.finish_node();
    }
    
    fn parse_parameter_list(&mut self) {
        self.builder.start_node(SyntaxKind::PARAM_LIST.into());
        
        if !self.expect(SyntaxKind::L_PAREN) {
            self.builder.finish_node();
            return;
        }
        
        self.skip_trivia();
        
        while !self.at(SyntaxKind::R_PAREN) && !self.at_end() {
            self.parse_parameter();
            self.skip_trivia();
            
            if self.at(SyntaxKind::COMMA) {
                self.advance();
                self.skip_trivia();
            } else if !self.at(SyntaxKind::R_PAREN) {
                self.error("expected ',' or ')'".to_string());
                break;
            }
        }
        
        self.expect(SyntaxKind::R_PAREN);
        self.builder.finish_node();
    }
}
```

## Error Recovery

### Recovery Strategies

```rust
impl<'a> Parser<'a> {
    fn recover_to_item_boundary(&mut self) {
        // Create an error node for the problematic content
        self.builder.start_node(SyntaxKind::ERROR.into());
        
        // Consume tokens until we find a likely item start
        while !self.at_end() && !self.at_item_start() {
            self.advance();
        }
        
        self.builder.finish_node();
    }
    
    fn recover_to_expression_boundary(&mut self) {
        self.builder.start_node(SyntaxKind::ERROR.into());
        
        // Consume one token to make progress
        if !self.at_end() {
            self.advance();
        }
        
        self.builder.finish_node();
    }
    
    fn recover_in_function(&mut self) {
        // Try to find the parameter list or body
        while !self.at_end() && 
              !self.at(SyntaxKind::L_PAREN) && 
              !self.at(SyntaxKind::L_BRACE) {
            self.advance();
        }
    }
    
    fn at_item_start(&self) -> bool {
        matches!(self.current().map(|t| t.kind), 
            Some(SyntaxKind::FN_KW) | 
            Some(SyntaxKind::STRUCT_KW) | 
            Some(SyntaxKind::ENUM_KW) |
            Some(SyntaxKind::IMPL_KW))
    }
}
```

### Synchronization Points

```rust
impl<'a> Parser<'a> {
    fn synchronize(&mut self) {
        // Skip to the next synchronization point
        while !self.at_end() && !self.at_sync_point() {
            self.advance();
        }
    }
    
    fn at_sync_point(&self) -> bool {
        match self.current().map(|t| t.kind) {
            // Statement boundaries
            Some(SyntaxKind::SEMICOLON) |
            Some(SyntaxKind::L_BRACE) |
            Some(SyntaxKind::R_BRACE) |
            // Item keywords
            Some(SyntaxKind::FN_KW) |
            Some(SyntaxKind::LET_KW) |
            Some(SyntaxKind::IF_KW) => true,
            _ => false,
        }
    }
}
```

## Whitespace and Comment Handling

### Trivia Preservation

```rust
impl<'a> Parser<'a> {
    fn skip_trivia(&mut self) {
        while self.at_trivia() {
            self.advance();
        }
    }
    
    fn at_trivia(&self) -> bool {
        matches!(self.current().map(|t| t.kind),
            Some(SyntaxKind::WHITESPACE) |
            Some(SyntaxKind::COMMENT) |
            Some(SyntaxKind::LINE_COMMENT))
    }
    
    fn parse_with_trivia<F>(&mut self, f: F) 
    where 
        F: FnOnce(&mut Self)
    {
        self.skip_trivia();
        f(self);
        self.skip_trivia();
    }
}
```

### Leading and Trailing Trivia

```rust
impl<'a> Parser<'a> {
    fn parse_statement(&mut self) {
        // Preserve leading trivia
        self.skip_trivia();
        
        let checkpoint = self.builder.checkpoint();
        
        match self.current().map(|t| t.kind) {
            Some(SyntaxKind::LET_KW) => {
                self.builder.start_node_at(checkpoint, SyntaxKind::LET_STMT.into());
                self.parse_let_statement_inner();
                self.builder.finish_node();
            }
            Some(SyntaxKind::RETURN_KW) => {
                self.builder.start_node_at(checkpoint, SyntaxKind::RETURN_STMT.into());
                self.parse_return_statement_inner();
                self.builder.finish_node();
            }
            _ => {
                // Expression statement
                self.builder.start_node_at(checkpoint, SyntaxKind::EXPR_STMT.into());
                self.parse_expression();
                if self.at(SyntaxKind::SEMICOLON) {
                    self.advance();
                }
                self.builder.finish_node();
            }
        }
    }
}
```

## Integration with Parser Generators

### LALRPOP Integration

```rust
// In your LALRPOP grammar file
use rowan::{GreenNodeBuilder, GreenNode};

grammar<'input>(builder: &mut GreenNodeBuilder<'input>);

pub Program: () = {
    <items:Item*> => {
        builder.start_node(SyntaxKind::ROOT.into());
        // Items are already added to builder
        builder.finish_node();
    }
}

Item: () = {
    Function => {},
    Struct => {},
}

Function: () = {
    "fn" <name:Ident> "(" <params:ParamList?> ")" <body:Block> => {
        builder.start_node(SyntaxKind::FUNCTION.into());
        // Tokens and child nodes are already added
        builder.finish_node();
    }
}

// Custom token handling
extern {
    type Location = usize;
    type Error = LexError;
    
    enum Token {
        "fn" => Token::FnKw,
        "(" => Token::LParen,
        ")" => Token::RParen,
        Ident => Token::Ident(<String>),
    }
}
```

### Pest Integration

```rust
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct MyParser;

fn parse_with_pest(input: &str) -> Result<GreenNode, pest::error::Error<Rule>> {
    let pairs = MyParser::parse(Rule::program, input)?;
    let mut builder = GreenNodeBuilder::new();
    
    build_tree_from_pairs(&mut builder, pairs);
    
    Ok(builder.finish())
}

fn build_tree_from_pairs(
    builder: &mut GreenNodeBuilder,
    pairs: pest::iterators::Pairs<Rule>
) {
    for pair in pairs {
        match pair.as_rule() {
            Rule::program => {
                builder.start_node(SyntaxKind::ROOT.into());
                build_tree_from_pairs(builder, pair.into_inner());
                builder.finish_node();
            }
            Rule::function => {
                builder.start_node(SyntaxKind::FUNCTION.into());
                build_tree_from_pairs(builder, pair.into_inner());
                builder.finish_node();
            }
            Rule::ident => {
                builder.token(SyntaxKind::IDENT.into(), pair.as_str());
            }
            Rule::WHITESPACE => {
                builder.token(SyntaxKind::WHITESPACE.into(), pair.as_str());
            }
            _ => {
                build_tree_from_pairs(builder, pair.into_inner());
            }
        }
    }
}
```

## Streaming Parser Integration

### Incremental Token Processing

```rust
struct StreamingParser<'a> {
    builder: GreenNodeBuilder<'a>,
    state_stack: Vec<ParseState>,
    errors: Vec<ParseError>,
}

#[derive(Debug, Clone)]
enum ParseState {
    Root,
    Function { has_name: bool, has_params: bool },
    Block { brace_count: usize },
    Expression { paren_depth: usize },
}

impl<'a> StreamingParser<'a> {
    fn new() -> Self {
        let mut parser = Self {
            builder: GreenNodeBuilder::new(),
            state_stack: Vec::new(),
            errors: Vec::new(),
        };
        
        parser.builder.start_node(SyntaxKind::ROOT.into());
        parser.state_stack.push(ParseState::Root);
        
        parser
    }
    
    fn feed_token(&mut self, token: Token) {
        match self.current_state() {
            ParseState::Root => self.handle_root_token(token),
            ParseState::Function { .. } => self.handle_function_token(token),
            ParseState::Block { .. } => self.handle_block_token(token),
            ParseState::Expression { .. } => self.handle_expression_token(token),
        }
    }
    
    fn finish(mut self) -> ParseResult {
        // Close any remaining open nodes
        while self.state_stack.len() > 1 {
            self.pop_state();
        }
        
        self.builder.finish_node(); // ROOT
        
        ParseResult {
            green: self.builder.finish(),
            errors: self.errors,
        }
    }
    
    fn current_state(&self) -> &ParseState {
        self.state_stack.last().unwrap()
    }
    
    fn push_state(&mut self, state: ParseState) {
        self.state_stack.push(state);
    }
    
    fn pop_state(&mut self) {
        if self.state_stack.len() > 1 {
            let state = self.state_stack.pop().unwrap();
            match state {
                ParseState::Function { .. } => self.builder.finish_node(),
                ParseState::Block { .. } => self.builder.finish_node(),
                ParseState::Expression { .. } => self.builder.finish_node(),
                _ => {}
            }
        }
    }
}
```

## Advanced Parser Patterns

### Macro Expansion Integration

```rust
struct MacroExpandingParser<'a> {
    base_parser: Parser<'a>,
    macro_definitions: HashMap<String, MacroDefinition>,
}

impl<'a> MacroExpandingParser<'a> {
    fn parse_macro_call(&mut self, name: &str) -> Option<GreenNode> {
        if let Some(macro_def) = self.macro_definitions.get(name) {
            // Expand macro and parse the result
            let expanded = self.expand_macro(macro_def);
            let tokens = lex(&expanded);
            
            let mut sub_parser = Parser::new(&tokens);
            Some(sub_parser.parse_expression())
        } else {
            None
        }
    }
    
    fn handle_macro_invocation(&mut self) {
        let checkpoint = self.base_parser.builder.checkpoint();
        
        if let Some(name) = self.parse_macro_name() {
            if let Some(expanded_tree) = self.parse_macro_call(&name) {
                // Insert the expanded tree
                self.base_parser.builder.start_node_at(
                    checkpoint, 
                    SyntaxKind::MACRO_CALL.into()
                );
                self.base_parser.builder.add_child(expanded_tree.into());
                self.base_parser.builder.finish_node();
                return;
            }
        }
        
        // Fallback: treat as regular identifier
        self.base_parser.advance();
    }
}
```

### Multi-Language Support

```rust
trait LanguageParser {
    type Language: Language;
    
    fn parse_embedded_block(
        &mut self, 
        builder: &mut GreenNodeBuilder,
        content: &str
    ) -> Result<(), ParseError>;
}

struct MultiLanguageParser {
    primary_parser: RustParser,
    sql_parser: SqlParser,
    html_parser: HtmlParser,
}

impl MultiLanguageParser {
    fn parse_string_literal(&mut self, content: &str) -> GreenNode {
        // Detect embedded languages in string literals
        if content.starts_with("sql!") {
            self.parse_sql_literal(content)
        } else if content.starts_with("html!") {
            self.parse_html_literal(content)
        } else {
            self.parse_regular_string(content)
        }
    }
    
    fn parse_sql_literal(&mut self, content: &str) -> GreenNode {
        let mut builder = GreenNodeBuilder::new();
        
        builder.start_node(SyntaxKind::SQL_LITERAL.into());
        
        // Parse the SQL content with the SQL parser
        if let Err(e) = self.sql_parser.parse_embedded_block(&mut builder, content) {
            // Fall back to treating as regular string
            builder.token(SyntaxKind::STRING.into(), content);
        }
        
        builder.finish_node();
        builder.finish()
    }
}
```

## Testing Parser Integration

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    fn parse_and_check(input: &str, expected_kind: SyntaxKind) {
        let tokens = lex(input);
        let parser = Parser::new(&tokens);
        let result = parser.parse();
        
        assert!(result.errors.is_empty(), "Parse errors: {:?}", result.errors);
        
        let syntax = SyntaxNode::new_root(result.green);
        assert_eq!(syntax.kind(), SyntaxKind::ROOT);
        
        let first_child = syntax.first_child().unwrap();
        assert_eq!(first_child.kind(), expected_kind);
    }
    
    #[test]
    fn test_function_parsing() {
        parse_and_check("fn foo() {}", SyntaxKind::FUNCTION);
    }
    
    #[test]
    fn test_error_recovery() {
        let input = "fn { fn bar() {}";
        let tokens = lex(input);
        let parser = Parser::new(&tokens);
        let result = parser.parse();
        
        // Should have errors but still produce a tree
        assert!(!result.errors.is_empty());
        
        let syntax = SyntaxNode::new_root(result.green);
        
        // Should contain both an error node and a valid function
        let children: Vec<_> = syntax.children().collect();
        assert!(children.iter().any(|c| c.kind() == SyntaxKind::ERROR));
        assert!(children.iter().any(|c| c.kind() == SyntaxKind::FUNCTION));
    }
}
```

### Property-Based Testing

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn parser_never_panics(input in ".*") {
        let tokens = lex(&input);
        let parser = Parser::new(&tokens);
        let _result = parser.parse(); // Should never panic
    }
    
    #[test]
    fn parsed_text_equals_input(input in valid_program_regex()) {
        let tokens = lex(&input);
        let parser = Parser::new(&tokens);
        let result = parser.parse();
        
        let syntax = SyntaxNode::new_root(result.green);
        assert_eq!(syntax.text().to_string(), input);
    }
}

fn valid_program_regex() -> impl Strategy<Value = String> {
    // Generate valid program strings
    prop::collection::vec(
        prop_oneof![
            "fn foo() {}",
            "let x = 42;",
            "if true { }",
        ],
        1..10
    ).prop_map(|parts| parts.join("\n"))
}
```

## Performance Optimization

### Parser Caching

```rust
struct CachingParser<'a> {
    base_parser: Parser<'a>,
    node_cache: NodeCache,
    parse_cache: HashMap<(usize, SyntaxKind), GreenNode>,
}

impl<'a> CachingParser<'a> {
    fn parse_cached(&mut self, start_pos: usize, kind: SyntaxKind) -> Option<GreenNode> {
        if let Some(cached) = self.parse_cache.get(&(start_pos, kind)) {
            return Some(cached.clone());
        }
        
        let checkpoint = self.base_parser.pos;
        let result = match kind {
            SyntaxKind::EXPRESSION => self.parse_expression(),
            SyntaxKind::STATEMENT => self.parse_statement(),
            _ => return None,
        };
        
        if self.base_parser.pos > checkpoint {
            self.parse_cache.insert((start_pos, kind), result.clone());
        }
        
        Some(result)
    }
}
```

The key to successful parser integration with Rowan is to embrace the lossless nature of the trees and build them incrementally as you parse, rather than trying to construct them after parsing is complete.