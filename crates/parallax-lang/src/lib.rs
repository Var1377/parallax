pub mod ast;
pub mod parser;
pub mod error;
pub mod location;
pub mod visitor;

pub use error::ParallaxError;
pub use visitor::Visitor;
use tree_sitter::{Parser, Tree, Node};

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

    /// Parse and validate a function definition
    pub fn parse_function(&mut self, source: &str) -> Result<FunctionInfo, ParallaxError> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        
        // Find the first function definition
        let function_node = find_first_node(&root, "function")
            .ok_or_else(|| ParallaxError::NodeError { 
                message: "No function found".to_string(),
                span: None,
                node_type: "function".to_string(),
            })?;
        
        // Extract function information
        let name = find_first_node(&function_node, "identifier")
            .and_then(|n| Some(n.utf8_text(source.as_bytes()).ok()?.to_string()))
            .ok_or_else(|| ParallaxError::NodeError {
                message: "Could not find function name".to_string(),
                span: None,
                node_type: "identifier".to_string(),
            })?;

        let return_type = find_first_node(&function_node, "type")
            .and_then(|n| Some(n.utf8_text(source.as_bytes()).ok()?.to_string()))
            .unwrap_or_else(|| "()".to_string());

        Ok(FunctionInfo { name, return_type })
    }
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub name: String,
    pub return_type: String,
}

/// Helper function to find the first node of a given type in the tree
fn find_first_node<'a>(node: &Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut cursor = node.walk();
    if node.kind() == kind {
        return Some(*node);
    }
    
    if cursor.goto_first_child() {
        loop {
            let current = cursor.node();
            if current.kind() == kind {
                return Some(current);
            }
            
            if let Some(found) = find_first_node(&current, kind) {
                return Some(found);
            }
            
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let mut parser = ParallaxParser::new().unwrap();
        let source = "fn add(x: i32, y: i32) -> i32 = { x + y };";
        let result = parser.parse_function(source).unwrap();
        assert_eq!(result.name, "add");
        assert_eq!(result.return_type, "i32");
    }

    #[test]
    fn test_parse_error_handling() {
        let mut parser = ParallaxParser::new().unwrap();
        let source = "fn invalid syntax {";
        assert!(parser.parse_function(source).is_err());
    }
}
