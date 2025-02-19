pub mod token;
pub mod lexer;

pub use token::*;
pub use lexer::*;

#[cfg(test)]
mod tests {
    use crate::lexer::{lex, TokenKind};

    #[test]
    fn test_lex_simple_symbols() {
        let input = "@foo = 123 & [*] ~";
        let tokens = lex(input).unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind.clone()).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::AtSign,
                TokenKind::Alphanumeric,
                TokenKind::Eq,
                TokenKind::Number,
                TokenKind::Ampersand,
                TokenKind::LBracket,
                TokenKind::Star,
                TokenKind::RBracket,
                TokenKind::Tilde
            ]
        );
    }

    #[test]
    fn test_lex_operator_tokens() {
        let input = ":>> :<< :- :/ :% >> << + - * / % = ! < > & | ^";
        let tokens = lex(input).unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind.clone()).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::ColonShr,
                TokenKind::ColonShl,
                TokenKind::ColonMinus,
                TokenKind::ColonSlash,
                TokenKind::ColonPercent,
                TokenKind::Shr,
                TokenKind::Shl,
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::Eq,
                TokenKind::Bang,
                TokenKind::Lt,
                TokenKind::Gt,
                TokenKind::Ampersand,
                TokenKind::Pipe,
                TokenKind::Caret,
            ]
        );
    }

    #[test]
    fn test_lex_operator_brackets() {
        let input = "[+] [*] [-]";
        let tokens = lex(input).unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind.clone()).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::LBracket,
                TokenKind::Plus,
                TokenKind::RBracket,
                TokenKind::LBracket,
                TokenKind::Star,
                TokenKind::RBracket,
                TokenKind::LBracket,
                TokenKind::Minus,
                TokenKind::RBracket,
            ]
        );
    }

    #[test]
    fn test_lex_compound_operators() {
        let input = ":>> :<< :- :/ :%";
        let tokens = lex(input).unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind.clone()).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::ColonShr,
                TokenKind::ColonShl,
                TokenKind::ColonMinus,
                TokenKind::ColonSlash,
                TokenKind::ColonPercent,
            ]
        );
    }

    #[test]
    fn test_lex_various_tokens() {
        let input = "@foo = 123 & [*] ~ (a b) {c d} $(e f) ?(g h)";
        let tokens = lex(input).unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind.clone()).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::AtSign,
                TokenKind::Alphanumeric,
                TokenKind::Eq,
                TokenKind::Number,
                TokenKind::Ampersand,
                TokenKind::LBracket,
                TokenKind::Star,
                TokenKind::RBracket,
                TokenKind::Tilde,
                TokenKind::LParen,
                TokenKind::Alphanumeric,
                TokenKind::Alphanumeric,
                TokenKind::RParen,
                TokenKind::LBrace,
                TokenKind::Alphanumeric,
                TokenKind::Alphanumeric,
                TokenKind::RBrace,
                TokenKind::DollarLParen,
                TokenKind::Alphanumeric,
                TokenKind::Alphanumeric,
                TokenKind::RParen,
                TokenKind::QuestionLParen,
                TokenKind::Alphanumeric,
                TokenKind::Alphanumeric,
                TokenKind::RParen,
            ]
        );
    }

    #[test]
    fn test_lex_invalid_token() {
        let input = "@foo = 123 & [*] ~ #$%^&";
        let result = lex(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_lex_empty_input() {
        let input = "";
        let tokens = lex(input).unwrap();
        assert!(tokens.is_empty());
    }
}
