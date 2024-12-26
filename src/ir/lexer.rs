use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{anychar, char},
    combinator::map,
    multi::many0,
    IResult,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<'s> {
    // Multi-char
    DollarParen,    // "$("
    QuestionParen,  // "?("
    ColonDash,      // ":-"
    ColonSlash,     // ":/"
    ColonPercent,   // ":%"
    ColonShr,       // ":>>"
    ColonShl,       // ":<<"
    Le,             // "<="
    Ge,             // ">="
    Shr,            // ">>"
    Shl,            // "<<"
    // Single-char
    LParen,         // '('
    RParen,         // ')'
    LBrace,         // '{'
    RBrace,         // '}'
    LBracket,       // '[''
    RBracket,       // ']'
    Equals,         // '='
    Tilde,          // '~'
    Star,           // '*'
    At,             // '@'
    Plus,           // '+'
    Minus,          // '-'
    Slash,          // '/'
    Percent,        // '%'
    Bang,           // '!'
    Lt,             // '<'
    Gt,             // '>'
    Ampersand,      // '&'
    Pipe,           // '|'
    Caret,          // '^'
    // Values
    Identifier(&'s str),
    Number(&'s str),
    Dot,            // separate token for '.'
    Whitespace(&'s str),
    // Fallback
    Error(char),
}

// Parse multi-char tokens
fn parse_multi_char(input: &str) -> IResult<&str, Token> {
    alt((
        map(tag("$("), |_| Token::DollarParen),
        map(tag("?("), |_| Token::QuestionParen),
        map(tag(":-"), |_| Token::ColonDash),
        map(tag(":/"), |_| Token::ColonSlash),
        map(tag(":%"), |_| Token::ColonPercent),
        map(tag(":>>"), |_| Token::ColonShr),
        map(tag(":<<"), |_| Token::ColonShl),
        map(tag("<="), |_| Token::Le),
        map(tag(">="), |_| Token::Ge),
        map(tag(">>"), |_| Token::Shr),
        map(tag("<<"), |_| Token::Shl),
    ))(input)
}

// Parse single-char tokens
fn parse_single_char(input: &str) -> IResult<&str, Token> {
    alt((
        map(char('('), |_| Token::LParen),
        map(char(')'), |_| Token::RParen),
        map(char('{'), |_| Token::LBrace),
        map(char('}'), |_| Token::RBrace),
        map(char('['), |_| Token::LBracket),
        map(char(']'), |_| Token::RBracket),
        map(char('='), |_| Token::Equals),
        map(char('~'), |_| Token::Tilde),
        map(char('*'), |_| Token::Star),
        map(char('@'), |_| Token::At),
        map(char('+'), |_| Token::Plus),
        map(char('-'), |_| Token::Minus),
        map(char('/'), |_| Token::Slash),
        map(char('%'), |_| Token::Percent),
        map(char('!'), |_| Token::Bang),
        map(char('<'), |_| Token::Lt),
        map(char('>'), |_| Token::Gt),
        map(char('&'), |_| Token::Ampersand),
        map(char('|'), |_| Token::Pipe),
        map(char('^'), |_| Token::Caret),
    ))(input)
}

// Parse dot
fn parse_dot(input: &str) -> IResult<&str, Token> {
    map(char('.'), |_| Token::Dot)(input)
}

// Parse number
fn parse_number(input: &str) -> IResult<&str, Token> {
    let (input, digits) = take_while1(|c: char| c.is_ascii_digit())(input)?;
    Ok((input, Token::Number(digits)))
}

// Parse identifier
fn parse_identifier(input: &str) -> IResult<&str, Token> {
    map(
        take_while1(|c: char| c.is_ascii_alphanumeric() || "_./-".contains(c)),
        |s: &str| Token::Identifier(s),
    )(input)
}

fn parse_whitespace(input: &str) -> IResult<&str, Token> {
    use nom::character::complete::multispace1;
    map(multispace1, |ws: &str| Token::Whitespace(ws))(input)
}

// Parse a single token, falling back to Error if nothing matches
fn parse_token(input: &str) -> IResult<&str, Token> {
    alt((
        parse_multi_char,
        parse_single_char,
        parse_dot,
        parse_number,
        parse_identifier,
        parse_whitespace,
        // Fallback: consume one char as error
        map(anychar, Token::Error),
    ))(input)
}

// Main lexer
pub fn lex(input: &str) -> IResult<&str, Vec<Token>> {
    many0(parse_token)(input)
}
#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(input: &str) -> Vec<Token> {
        let (_, tokens) = lex(input).unwrap();
        tokens
    }

    #[test]
    fn test_multi_char_tokens() {
        let input = "$( ?( :- :/ :% :>> :<< <= >= >> <<";
        let expected = vec![
            Token::DollarParen,
            Token::Whitespace(" "),
            Token::QuestionParen,
            Token::Whitespace(" "),
            Token::ColonDash,
            Token::Whitespace(" "),
            Token::ColonSlash,
            Token::Whitespace(" "),
            Token::ColonPercent,
            Token::Whitespace(" "),
            Token::ColonShr,
            Token::Whitespace(" "),
            Token::ColonShl,
            Token::Whitespace(" "),
            Token::Le,
            Token::Whitespace(" "),
            Token::Ge,
            Token::Whitespace(" "),
            Token::Shr,
            Token::Whitespace(" "),
            Token::Shl,
        ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_single_char_tokens() {
        let input = "( ) { } [ ] = ~ * @ + - / % ! < > & | ^";
        let expected = vec![
            Token::LParen,
            Token::Whitespace(" "),
            Token::RParen,
            Token::Whitespace(" "),
            Token::LBrace,
            Token::Whitespace(" "),
            Token::RBrace,
            Token::Whitespace(" "),
            Token::LBracket,
            Token::Whitespace(" "),
            Token::RBracket,
            Token::Whitespace(" "),
            Token::Equals,
            Token::Whitespace(" "),
            Token::Tilde,
            Token::Whitespace(" "),
            Token::Star,
            Token::Whitespace(" "),
            Token::At,
            Token::Whitespace(" "),
            Token::Plus,
            Token::Whitespace(" "),
            Token::Minus,
            Token::Whitespace(" "),
            Token::Slash,
            Token::Whitespace(" "),
            Token::Percent,
            Token::Whitespace(" "),
            Token::Bang,
            Token::Whitespace(" "),
            Token::Lt,
            Token::Whitespace(" "),
            Token::Gt,
            Token::Whitespace(" "),
            Token::Ampersand,
            Token::Whitespace(" "),
            Token::Pipe,
            Token::Whitespace(" "),
            Token::Caret,
        ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_numbers_and_dot() {
        let input = "123.456";
        let expected = vec![
            Token::Number("123"),
            Token::Dot,
            Token::Number("456"),
        ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_identifiers() {
        let input = "abc _def ghi.jkl mno-pqr stu/vwx";
        let expected = vec![
            Token::Identifier("abc"),
            Token::Whitespace(" "),
            Token::Identifier("_def"),
            Token::Whitespace(" "),
            Token::Identifier("ghi.jkl"),
            Token::Whitespace(" "),
            Token::Identifier("mno-pqr"),
            Token::Whitespace(" "),
            Token::Identifier("stu/vwx"),
        ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_whitespace_tokens() {
        let input = "  \t\nabc   123  \n";
        let tokens = lex_all(input);
        // Make sure we see Whitespace, Identifier, Whitespace, Number, Whitespace
        assert_eq!(
            tokens,
            vec![
                Token::Whitespace("  \t\n"),
                Token::Identifier("abc"),
                Token::Whitespace("   "),
                Token::Number("123"),
                Token::Whitespace("  \n"),
            ]
        );
    }

    #[test]
    fn test_mixed_tokens() {
        let input = "  $(  abc  123.456  )  ";
        let expected = vec![
            Token::Whitespace("  "),
            Token::DollarParen,
            Token::Whitespace("  "),
            Token::Identifier("abc"),
            Token::Whitespace("  "),
            Token::Number("123"),
            Token::Dot,
            Token::Number("456"),
            Token::Whitespace("  "),
            Token::RParen,
            Token::Whitespace("  "),
        ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_error_token() {
        let input = "$( abc @ 123.456 ) #";
        let expected = vec![
            Token::DollarParen,
            Token::Whitespace(" "),
            Token::Identifier("abc"),
            Token::Whitespace(" "),
            Token::At,
            Token::Whitespace(" "),
            Token::Number("123"),
            Token::Dot,
            Token::Number("456"),
            Token::Whitespace(" "),
            Token::RParen,
            Token::Whitespace(" "),
            Token::Error('#'),
        ];
        assert_eq!(lex_all(input), expected);
    }
}