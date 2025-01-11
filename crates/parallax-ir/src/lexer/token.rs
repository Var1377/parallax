use logos::Logos;
use miette::SourceSpan;

/// A token spans from `start` to `end` within the original source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub span: SourceSpan,
}

/// All possible tokens we can encounter in our grammar.
#[derive(Debug, Logos, PartialEq, Eq, Clone)]
pub enum TokenKind {
    // Multi-character operators
    #[token(":>>")]
    ColonShr,
    #[token(":<<")]
    ColonShl,
    #[token(":-")]
    ColonMinus,
    #[token(":/")]
    ColonSlash,
    #[token(":%")]
    ColonPercent,

    #[token(">>")]
    Shr,
    #[token("<<")]
    Shl,

    // Single-character operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Eq,
    #[token("!")]
    Bang,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,

    // Punctuation
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("$(")]
    DollarLParen,
    #[token("?(")]
    QuestionLParen,
    #[token("~")]
    Tilde,
    #[token("@")]
    AtSign,

    // Numbers (Nat, Int, Float)
    #[regex(r"[0-9]+(\.[0-9]+)?")]
    Number,

    // Alphanumeric identifiers
    #[regex(r"[a-zA-Z][a-zA-Z0-9_.\-/]*")]
    Alphanumeric,

    // Whitespace (to be skipped)
    #[regex(r"[ \t\n\r]+", logos::skip)]
    Whitespace,

    // Catch-all for anything unexpected
    #[error]
    Error,
}
