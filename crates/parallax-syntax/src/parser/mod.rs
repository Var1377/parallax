pub mod expr;
pub mod pattern;
pub mod items;
pub mod types;
pub mod common;
pub mod validate;
pub mod structs;
pub mod literals;
pub mod calls;
pub mod scanner;

use tree_sitter::{Node, Parser, Tree};
use crate::error::SyntaxError;
use crate::ast::items::Item;
use miette::SourceSpan;
/// Result type for parser operations
pub type ParseResult<T> = Result<T, SyntaxError>;

/// Error node information
pub struct ErrorNodeInfo {
    pub kind: String,
    pub span: SourceSpan,
    pub source_text: String,
}

/// Represents the Parallax language parser
pub struct ParallaxParser {
    parser: Parser,
    /// Error recovery settings
    recovery_enabled: bool,
}

impl ParallaxParser {
    /// Parse Parallax source code and return the syntax tree
    pub fn parse(&mut self, source: &str) -> Result<Tree, SyntaxError> {
        self.parser.parse(source, None)
            .ok_or_else(|| SyntaxError::ParseError {
                message: "Failed to parse source code".to_string(),
                span: None,
            })
    }

    /// Parse source into AST
    pub fn parse_ast(&mut self, source: &str) -> (Vec<Item>, Vec<SyntaxError>) {
        match self.parse(source) {
            Ok(tree) => {
                let root = tree.root_node();
                parse_source_file(&root, source)
            },
            Err(e) => (Vec::new(), vec![e]),
        }
    }
    
    /// Enable or disable error recovery during parsing
    pub fn set_error_recovery(&mut self, enabled: bool) {
        self.recovery_enabled = enabled;
    }
    
    /// Check if a node has errors in it or its children
    pub fn has_errors(&self, node: &Node) -> bool {
        if node.is_error() || node.is_missing() {
            return true;
        }
        
        let mut cursor = node.walk();
        if cursor.goto_first_child() {
            loop {
                if self.has_errors(&cursor.node()) {
                    return true;
                }
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
        
        false
    }
    
    /// Get information about error nodes in the parse tree
    pub fn get_error_nodes(&self, node: &Node, source: &str) -> Vec<ErrorNodeInfo> {
        let mut errors = Vec::new();
        self.collect_error_nodes(node, source, &mut errors);
        errors
    }
    
    /// Helper method to collect error nodes
    fn collect_error_nodes<'a>(&self, node: &Node, source: &str, errors: &mut Vec<ErrorNodeInfo>) {
        if node.is_error() || node.is_missing() {
            let span = SourceSpan::new(node.start_byte().into(), node.end_byte() - node.start_byte());
            
            // Extract the node's text if possible
            let node_text = if node.start_byte() < node.end_byte() {
                node.utf8_text(source.as_bytes()).unwrap_or("").to_string()
            } else {
                "".to_string()
            };
            
            errors.push(ErrorNodeInfo {
                kind: node.kind().to_string(),
                span,
                source_text: node_text,
            });
        }
        
        let mut cursor = node.walk();
        if cursor.goto_first_child() {
            loop {
                self.collect_error_nodes(&cursor.node(), source, errors);
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
    }
    
    /// Format parsing errors for better diagnostics
    pub fn format_errors(&self, node: &Node, source: &str) -> Vec<SyntaxError> {
        let error_nodes = self.get_error_nodes(node, source);
        
        error_nodes.into_iter().map(|error_info| {
            let context = extract_error_context_for_span(&error_info.span, source, 10);
            SyntaxError::SyntaxError {
                message: format!("Syntax error while parsing {}", error_info.kind),
                span: Some(error_info.span),
                expected: None, 
                found: Some(context),
            }
        }).collect()
    }
}

/// Parse a source file into a list of items
pub fn parse_source_file(node: &Node, source: &str) -> (Vec<Item>, Vec<SyntaxError>) {
    let mut items = Vec::new();
    let mut errors = Vec::new();
    
    let mut cursor = node.walk();
    if cursor.goto_first_child() {
        loop {
            let current = cursor.node();
            if current.kind() == "item" {
                match items::parse_item(&current, source) {
                    Ok(item) => items.push(item),
                    Err(e) => errors.push(e),
                }
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    
    (items, errors)
}

/// Extract context around an error node for better error reporting
fn extract_error_context_for_span(span: &SourceSpan, source: &str, context_size: usize) -> String {
    let start = span.offset();
    let end = span.offset() + span.len();
    
    let source_bytes = source.as_bytes();
    
    // Find the start of the context (but don't go beyond the beginning of the source)
    let context_start = start.saturating_sub(context_size);
    
    // Find the end of the context (but don't go beyond the end of the source)
    let context_end = (end + context_size).min(source_bytes.len());
    
    // Extract the context and convert it to a string
    // Handle potential UTF-8 boundary issues
    let mut context = String::new();
    
    // Add prefix
    if context_start < start {
        if let Ok(prefix) = std::str::from_utf8(&source_bytes[context_start..start]) {
            context.push_str(prefix);
        }
    }
    
    // Add the problematic text
    if start < end {
        if let Ok(error_text) = std::str::from_utf8(&source_bytes[start..end]) {
            context.push_str(error_text);
        }
    }
    
    // Add suffix
    if end < context_end {
        if let Ok(suffix) = std::str::from_utf8(&source_bytes[end..context_end]) {
            context.push_str(suffix);
        }
    }
    
    context
}

#[allow(dead_code)]
pub(crate) fn print_tree_structure(node: &Node, source: &str, max_depth: usize) {
    print_tree_structure_recursive(node, source, 0, max_depth);
}

#[allow(dead_code)]
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