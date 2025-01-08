mod ast;
pub mod lexer;
pub mod parser;

use chumsky::Parser;
pub use ast::*;
pub use lexer::*;
pub use parser::*;

pub fn parse(input: &str) -> Result<Book, String> {
    let (_, tokens) = lexer::lex(input).map_err(|e| format!("Lexer error: {:?}", e))?;
    parser::parser().parse(tokens).map_err(|e| format!("Parser error: {:?}", e))
}

pub fn generate_graph(book: Book) -> Result<String, String> {
    todo!("Implement network graph generation")
}

pub fn analyze_stats(book: Book) -> Result<String, String> {
    todo!("Implement statistics analysis")
}