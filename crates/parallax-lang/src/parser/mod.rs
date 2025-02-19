pub mod expr;
pub mod pattern;
pub mod items;
pub mod types;
pub mod common;

use tree_sitter::{Node, Parser, Tree};
use crate::error::ParallaxError;
use crate::ast::items::Item;

pub struct ParallaxParser {
    parser: Parser,
}

impl ParallaxParser {
    /// Create a new Parallax parser
    pub fn new() -> Result<Self, ParallaxError> {
        let mut parser = Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;
        Ok(Self { parser })
    }

    /// Parse Parallax source code and return the syntax tree
    pub fn parse(&mut self, source: &str) -> Result<Tree, ParallaxError> {
        self.parser.parse(source, None)
            .ok_or_else(|| ParallaxError::ParseError {
                message: "Failed to parse source code".to_string(),
                span: None,
            })
    }

    /// Parse source into AST
    pub fn parse_ast(&mut self, source: &str) -> Result<Vec<Item>, ParallaxError> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        parse_source_file(&root, source)
    }
}

/// Parse a source file into a list of items
pub fn parse_source_file(node: &Node, source: &str) -> Result<Vec<Item>, ParallaxError> {
    let mut items = Vec::new();
    let mut cursor = node.walk();
    
    if cursor.goto_first_child() {
        loop {
            let current = cursor.node();
            if current.kind() == "item" {
                items.push(items::parse_item(&current, source)?);
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    
    Ok(items)
}

pub(crate) fn print_tree_structure(node: &Node, source: &str, max_depth: usize) {
    print_tree_structure_recursive(node, source, 0, max_depth);
}

fn print_tree_structure_recursive(node: &Node, source: &str, depth: usize, max_depth: usize) {
    if depth >= max_depth {
        return;
    }

    let indent = " ".repeat(depth * 4);
    let text = if node.child_count() == 0 {
        format!(" {}", node.utf8_text(source.as_bytes()).unwrap_or(""))
    } else {
        String::new()
    };

    println!("{}{} [{}-{}]{}",
        indent,
        node.kind(),
        node.start_byte(),
        node.end_byte(),
        text
    );

    let mut cursor = node.walk();
    if cursor.goto_first_child() {
        loop {
            print_tree_structure_recursive(&cursor.node(), source, depth + 1, max_depth);
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
}