use super::token::*;
use crate::{IRResult, IRError};
use miette::SourceSpan;
use logos::Logos;

/// Lexes the input string into a vector of tokens.
pub fn lex(input: &str) -> IRResult<Vec<Token<'_>>> {
    let mut lexer = TokenKind::lexer(input);
    let mut tokens = Vec::new();

    while let Some(kind) = lexer.next() {
        let range = lexer.span();
        let lexeme = &input[range.clone()];
        let span = SourceSpan::new(range.start.into(), range.len());

        match kind {
            TokenKind::Error => {
                return Err(IRError::Lexer {
                    span,
                    message: format!("Unrecognized token: '{}'", lexeme),
                });
            }
            _ => {
                tokens.push(Token {
                    kind,
                    lexeme,
                    span,
                });
            }
        }
    }

    Ok(tokens)
}
