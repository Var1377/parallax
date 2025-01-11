use crate::ast::*;
use crate::{IRResult, IRError};
use crate::lexer::{Token, TokenKind};
use miette::SourceSpan;

/// Our hand-rolled parser structure.
pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self { tokens, pos: 0 }
    }

    /// Peek at the current token without consuming it.
    fn peek(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.pos)
    }

    /// Consume and return the current token.
    fn next(&mut self) -> Option<&Token<'a>> {
        let tok = self.tokens.get(self.pos);
        self.pos += 1;
        tok
    }

    /// Expect the next token to be of a specific kind.
    fn expect(&mut self, expected: TokenKind) -> IRResult<&Token<'a>> {
        match self.next() {
            Some(t) if t.kind == expected => Ok(t),
            Some(t) => {
                Err(IRError::Parser {
                    span: t.span,
                    message: format!("Expected {:?}, found {:?}", expected, t.kind),
                })
            }
            None => {
                // We ran out of tokens but were expecting something.
                Err(IRError::Parser {
                    span: SourceSpan::new(0.into(), 0usize),
                    message: format!("Unexpected end of input; expected {:?}", expected),
                })
            }
        }
    }

    //--------------------------------------------------------------------------
    // Top-level grammar: <Book> ::= ("@" <name> "=" <Net>)*
    //--------------------------------------------------------------------------

    pub fn parse_book(&mut self) -> IRResult<Book<'a>> {
        let mut definitions = Vec::new();

        while let Some(tok) = self.peek() {
            if tok.kind == TokenKind::AtSign {
                // Each definition is: @ <name> = <Net>
                let def = self.parse_definition()?;
                definitions.push(def);
            } else {
                // If it doesn't start with '@', we've finished all definitions.
                break;
            }
        }

        Ok(Book { definitions })
    }

    /// Parse a single definition: @ <name> = <Net>
fn parse_definition(&mut self) -> IRResult<Definition<'a>> {
    // Expect '@'
    self.expect(TokenKind::AtSign)?;

    // Read the name and immediately extract the lexeme to end the borrow
    let name = {
        let name_tok = self.expect(TokenKind::Alphanumeric)?;
        name_tok.lexeme
    };

    // Expect '='
    self.expect(TokenKind::Eq)?;

    // Parse the <Net>
    let net = self.parse_net()?;

    Ok(Definition {
        name,
        net,
    })
}


    //--------------------------------------------------------------------------
    // <Net> ::= <Tree> ("&" <Redex>)*
    //--------------------------------------------------------------------------

    fn parse_net(&mut self) -> IRResult<Net<'a>> {
        // First parse a single <Tree>
        let tree = self.parse_tree()?;
        // Then parse zero or more "& <Redex>"
        let mut redexes = Vec::new();

        while let Some(tok) = self.peek() {
            if tok.kind == TokenKind::Ampersand {
                // consume '&'
                self.next();
                
                // Check for '!' after '&'
                let strict = if let Some(tok) = self.peek() {
                    if tok.kind == TokenKind::Bang {
                        self.next();  // consume '!'
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                
                // parse <Redex>
                let mut redex = self.parse_redex()?;
                redex.strict = strict;
                redexes.push(redex);
            } else {
                break;
            }
        }

        Ok(Net { tree, redexes })
    }

    //--------------------------------------------------------------------------
    // <Redex> ::= <Tree> "~" <Tree>
    //--------------------------------------------------------------------------

    pub fn parse_redex(&mut self) -> IRResult<Redex<'a>> {
        let left_tree = self.parse_tree()?;
        self.expect(TokenKind::Tilde)?;
        let right_tree = self.parse_tree()?;
        Ok(Redex {
            strict: false,
            left: left_tree,
            right: right_tree,
        })
    }

    //--------------------------------------------------------------------------
    // <Tree> ::= <alphanumeric> | <Node>
    //--------------------------------------------------------------------------

    pub fn parse_tree(&mut self) -> IRResult<Tree<'a>> {
        if let Some(tok) = self.peek() {
            match tok.kind {
                // If it's Alphanumeric, treat it as a variable name <Tree> ::= <alphanumeric>
                TokenKind::Alphanumeric => {
                    let var = self.next().unwrap(); // safe to unwrap here
                    Ok(Tree::Var(var.lexeme))
                }
                _ => {
                    // Otherwise, parse it as <Node>
                    let node = self.parse_node()?;
                    Ok(Tree::Node(node))
                }
            }
        } else {
            Err(IRError::Parser {
                span: SourceSpan::new(0.into(), 0usize),
                message: "Unexpected end of tokens while parsing <Tree>".into(),
            })
        }
    }

    //--------------------------------------------------------------------------
    // <Node> ::=
    //   "*"
    // | "@" <alphanumeric>
    // | <Numeric>
    // | "(" <Tree> <Tree> ")"
    // | "{" <Tree> <Tree> "}"
    // | "$(" <Tree> <Tree> ")"
    // | "?(" <Tree> <Tree> ")"
    //--------------------------------------------------------------------------

    fn parse_node(&mut self) -> IRResult<Node<'a>> {
        if let Some(tok) = self.peek() {
            match tok.kind {
                // "*" => Eraser
                TokenKind::Star => {
                    // By your grammar, a single '*' is an Eraser (ERA).
                    self.next(); // consume '*'
                    Ok(Node::Eraser)
                }

                // "@" <alphanumeric> => Reference
                TokenKind::AtSign => {
                    self.next(); // consume '@'
                    let name_tok = self.expect(TokenKind::Alphanumeric)?;
                    Ok(Node::Reference(name_tok.lexeme))
                }

                // "(" <Tree> <Tree> ")"
                TokenKind::LParen => {
                    self.next(); // consume '('
                    let left = self.parse_tree()?;
                    let right = self.parse_tree()?;
                    self.expect(TokenKind::RParen)?;
                    Ok(Node::Constructor(Box::new(left), Box::new(right)))
                }

                // "{" <Tree> <Tree> "}"
                TokenKind::LBrace => {
                    self.next(); // consume '{'
                    let left = self.parse_tree()?;
                    let right = self.parse_tree()?;
                    self.expect(TokenKind::RBrace)?;
                    Ok(Node::Duplicator(Box::new(left), Box::new(right)))
                }

                // "$(" <Tree> <Tree> ")"
                TokenKind::DollarLParen => {
                    self.next(); // consume '$('
                    let left = self.parse_tree()?;
                    let right = self.parse_tree()?;
                    self.expect(TokenKind::RParen)?;
                    Ok(Node::Operator(Box::new(left), Box::new(right)))
                }

                // "?(" <Tree> <Tree> ")"
                TokenKind::QuestionLParen => {
                    self.next(); // consume '?('
                    let left = self.parse_tree()?;
                    let right = self.parse_tree()?;
                    self.expect(TokenKind::RParen)?;
                    Ok(Node::Switch(Box::new(left), Box::new(right)))
                }

                // <Numeric> => either a <Number> or an <Operator>
                TokenKind::Number | TokenKind::LBracket => {
                    let numeric = self.parse_numeric()?;
                    Ok(Node::Numeric(numeric))
                }

                // Otherwise: unexpected token
                _ => {
                    Err(IRError::Parser {
                        span: tok.span,
                        message: format!("Unexpected token in <Node>: {:?}", tok.kind),
                    })
                }
            }
        } else {
            Err(IRError::Parser {
                span: SourceSpan::new(0.into(), 0usize),
                message: "Unexpected end of tokens while parsing <Node>".into(),
            })
        }
    }

    //--------------------------------------------------------------------------
    // <Numeric> ::=
    //   | <Number>
    //   | <Operator>
    //
    // <Number> ::=
    //   | <Nat>
    //   | <Int>
    //   | <Float>
    //   (Simplified to a single 'Number' token in this demo.)
    //--------------------------------------------------------------------------

    pub fn parse_numeric(&mut self) -> IRResult<Numeric<'a>> {
        if let Some(tok) = self.peek() {
            match tok.kind {
                // <Number> matched by a single token in the lexer
                TokenKind::Number => {
                    let t = self.next().unwrap();
                    Ok(Numeric::Number(t.lexeme))
                }
                // <Operator> => "[" <Operation> "]" or "[" <Operation> <Number> "]"
                TokenKind::LBracket => {
                    self.next(); // consume '['
                    let op_str = self.parse_operation_token()?;

                    // Possibly parse a trailing <Number> before closing bracket
                    let mut partial_num = None;
                    if let Some(t2) = self.peek() {
                        if t2.kind == TokenKind::Number {
                            let tnumber = self.next().unwrap();
                            partial_num = Some(tnumber.lexeme);
                        }
                    }

                    // expect closing ']'
                    self.expect(TokenKind::RBracket)?;

                    if let Some(num_str) = partial_num {
                        Ok(Numeric::Operator(
                            Operator::PartiallyApplied(op_str, num_str),
                        ))
                    } else {
                        Ok(Numeric::Operator(Operator::Unapplied(op_str)))
                    }
                }
                _ => {
                    Err(IRError::Parser {
                        span: tok.span,
                        message: format!(
                            "Expected <Number> or '[' for <Operator>, found {:?}",
                            tok.kind
                        ),
                    })
                }
            }
        } else {
            Err(IRError::Parser {
                span: SourceSpan::new(0.into(), 0usize),
                message: "Unexpected end of tokens while parsing <Numeric>".into(),
            })
        }
    }

    //--------------------------------------------------------------------------
    // <Operation> ::=
    //    "+" | "-" | "*" | "/" | "%" | "=" | "!" | "<" | ">" | "&" | "|" | "^"
    //    | ">>" | "<<" 
    //    | ":-" | ":/" | ":%" | ":>>" | ":<<"
    //
    // We treat each as one token in the lexer. parse_operation_token() just
    // consumes the next token if it is one of these "Operation" tokens.
    //--------------------------------------------------------------------------

    fn parse_operation_token(&mut self) -> IRResult<&'a str> {
        let tok = self.next().ok_or_else(|| IRError::Parser {
            span: SourceSpan::new(0.into(), 0usize),
            message: "Unexpected end of tokens while parsing <Operation>.".into(),
        })?;

        match tok.kind {
            // All operation tokens
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Eq
            | TokenKind::Bang
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::Ampersand
            | TokenKind::Pipe
            | TokenKind::Caret
            | TokenKind::Shr
            | TokenKind::Shl
            | TokenKind::ColonShr
            | TokenKind::ColonShl
            | TokenKind::ColonMinus
            | TokenKind::ColonSlash
            | TokenKind::ColonPercent
            => Ok(tok.lexeme),

            _ => {
                Err(IRError::Parser {
                    span: tok.span,
                    message: format!("Invalid <Operation> token: {:?}", tok.lexeme),
                })
            }
        }
    }

    // Function to parse Operator
    pub fn parse_operator(&mut self) -> IRResult<&'a str> {
        let tok = self.next().ok_or_else(|| IRError::Parser {
            span: SourceSpan::new(0.into(), 0usize),
            message: "Unexpected end of tokens while parsing <Operation>.".into(),
        })?;

        match tok.kind {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash | TokenKind::Percent | TokenKind::Eq | TokenKind::Bang | TokenKind::Lt | TokenKind::Gt | TokenKind::Ampersand | TokenKind::Pipe | TokenKind::Caret | TokenKind::Shr | TokenKind::Shl | TokenKind::ColonShr | TokenKind::ColonShl | TokenKind::ColonMinus | TokenKind::ColonSlash | TokenKind::ColonPercent => Ok(tok.lexeme),
            _ => {
                Err(IRError::Parser {
                    span: tok.span,
                    message: format!("Invalid <Operation> token: {:?}", tok.lexeme),
                })
            }
        }
    }
}

//------------------------------------------------------------------------------
// Public entry point for the parser.
//------------------------------------------------------------------------------

/// Parse an entire `<Book>` from the list of tokens.
pub fn parse_book<'a>(tokens: &'a [Token<'a>]) -> IRResult<Book<'a>> {
    let mut parser = Parser::new(tokens);
    parser.parse_book()
}
