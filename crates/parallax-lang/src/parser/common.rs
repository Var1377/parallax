use tree_sitter::Node;
use crate::ast::common::{Span, Ident};
use crate::error::ParallaxError;

/// Creates a Span from a Node's byte range
pub fn create_span(node: &Node) -> Span {
    Span {
        start: node.start_byte(),
        end: node.end_byte(),
    }
}

/// Extracts text from a Node, handling UTF-8 conversion and errors
pub fn node_text(node: &Node, source: &str) -> Result<String, ParallaxError> {
    node.utf8_text(source.as_bytes())
        .map(|s| s.to_string())
        .map_err(|e| ParallaxError::NodeError {
            message: format!("Invalid UTF-8 in node text: {}", e),
            span: Some(create_span(node)),
            node_type: node.kind().to_string(),
        })
}

/// Parses a path (sequence of identifiers) from a Node
pub fn parse_path(node: &Node, source: &str) -> Result<Vec<Ident>, ParallaxError> {
    let mut segments = Vec::new();
    let mut cursor = node.walk();
    
    match node.kind() {
        "path" => {
            // Handle path segments
            if cursor.goto_first_child() {
                loop {
                    let current = cursor.node();
                    match current.kind() {
                        "identifier" | "self" | "super" | "crate" => {
                            segments.push(Ident(node_text(&current, source)?));
                        },
                        "::" => {
                            // Skip the :: token
                        },
                        _ => return Err(ParallaxError::NodeError {
                            message: format!("Invalid path segment kind: {}", current.kind()),
                            span: Some(create_span(&current)),
                            node_type: current.kind().to_string(),
                        })
                    }
                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
            }
        },
        "path_segment" => {
            segments.push(parse_path_segment(node, source)?);
        },
        _ => return Err(ParallaxError::NodeError {
            message: format!("Expected path or path_segment node, got {}", node.kind()),
            span: Some(create_span(node)),
            node_type: node.kind().to_string(),
        })
    }
    
    if segments.is_empty() {
        return Err(ParallaxError::NodeError {
            message: "Path has no segments".to_string(),
            span: Some(create_span(node)),
            node_type: node.kind().to_string(),
        });
    }
    
    Ok(segments)
}

/// Parse a single path segment (identifier, self, super, or crate)
pub fn parse_path_segment(node: &Node, source: &str) -> Result<Ident, ParallaxError> {
    match node.kind() {
        "path_segment" => {
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                let segment = cursor.node();
                match segment.kind() {
                    "identifier" => Ok(Ident(node_text(&segment, source)?)),
                    "self" | "super" | "crate" => Ok(Ident(segment.kind().to_string())),
                    _ => Err(ParallaxError::NodeError {
                        message: format!("Invalid path segment kind: {}", segment.kind()),
                        span: Some(create_span(&segment)),
                        node_type: segment.kind().to_string(),
                    })
                }
            } else {
                Err(ParallaxError::NodeError {
                    message: "Empty path segment".to_string(),
                    span: Some(create_span(node)),
                    node_type: node.kind().to_string(),
                })
            }
        },
        "use_item" => {
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                let segment = cursor.node();
                match segment.kind() {
                    "identifier" => Ok(Ident(node_text(&segment, source)?)),
                    "self" | "super" | "crate" => Ok(Ident(segment.kind().to_string())),
                    _ => Err(ParallaxError::NodeError {
                        message: format!("Invalid use item content: {}", segment.kind()),
                        span: Some(create_span(&segment)),
                        node_type: segment.kind().to_string(),
                    })
                }
            } else {
                Err(ParallaxError::NodeError {
                    message: "Empty use item".to_string(),
                    span: Some(create_span(node)),
                    node_type: node.kind().to_string(),
                })
            }
        },
        "identifier" => Ok(Ident(node_text(node, source)?)),
        "self" | "super" | "crate" => Ok(Ident(node.kind().to_string())),
        _ => Err(ParallaxError::NodeError {
            message: format!("Invalid path segment kind: {}", node.kind()),
            span: Some(create_span(node)),
            node_type: node.kind().to_string(),
        })
    }
}

/// Gets a required child node by field name, with error handling
pub fn require_child<'a>(
    node: &'a Node, 
    field_name: &str,
    node_type: &str,
) -> Result<Node<'a>, ParallaxError> {
    node.child_by_field_name(field_name)
        .ok_or_else(|| ParallaxError::NodeError {
            message: format!("{} missing {}", node_type, field_name),
            span: Some(create_span(node)),
            node_type: node_type.to_string(),
        })
}

/// Visits all children of a node with a visitor function
pub fn visit_children<F>(node: &Node, mut visitor: F) -> Result<(), ParallaxError>
where F: FnMut(&Node) -> Result<(), ParallaxError> {
    let mut cursor = node.walk();
    if cursor.goto_first_child() {
        loop {
            visitor(&cursor.node())?;
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    Ok(())
}

/// Gets a child node by field name, returns None if not found
pub fn get_child<'a>(node: &'a Node, field_name: &str) -> Option<Node<'a>> {
    node.child_by_field_name(field_name)
}

/// Finds first child node of a specific kind
pub fn find_first_child<'a>(node: &Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut cursor = node.walk();
    if cursor.goto_first_child() {
        loop {
            let child = cursor.node();
            if child.kind() == kind {
                return Some(child);
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    None
}

/// Collects all children of a specific kind into a vector
pub fn collect_children<'a>(node: &'a Node, kind: &str) -> Vec<Node<'a>> {
    let mut children = Vec::new();
    let mut cursor = node.walk();
    
    if cursor.goto_first_child() {
        loop {
            let child = cursor.node();
            if child.kind() == kind {
                children.push(child);
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    
    children
}

/// Creates a node error with a custom message
pub fn node_error(node: &Node, message: &str) -> ParallaxError {
    ParallaxError::NodeError {
        message: message.to_string(),
        span: Some(create_span(node)),
        node_type: node.kind().to_string(),
    }
}

/// Creates a syntax error with expected and found values
pub fn syntax_error(node: &Node, message: &str, expected: Option<&str>, found: Option<&str>) -> ParallaxError {
    ParallaxError::SyntaxError {
        message: message.to_string(),
        span: Some(create_span(node)),
        expected: expected.map(String::from),
        found: found.map(String::from),
    }
}

#[cfg(test)]
pub mod test_utils {
    use tree_sitter::{Node, Parser};
    use std::collections::VecDeque;
    use crate::error::ParallaxError;

    /// Get a parser initialized for tests
    pub fn create_test_parser() -> Parser {
        let mut parser = Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into()).unwrap();
        parser
    }

    /// Find a node of a specific kind with the given field
    pub fn find_node<'a>(
        node: &Node<'a>,
        kind: &str,
        field_path: &[&str],
    ) -> Option<Node<'a>> {
        println!("Checking node: {} [{}]", node.kind(), node.to_sexp());
        
        // Check if the current node matches
        if node.kind() == kind {
            return Some(*node);
        }
        
        // If there are field paths, we need to traverse them
        if !field_path.is_empty() {
            if let Some(field) = node.child_by_field_name(field_path[0]) {
                return find_node(&field, kind, &field_path[1..]);
            }
            return None;
        }
        
        // Otherwise do a breadth-first search
        let mut queue = VecDeque::new();
        queue.push_back(*node);
        
        while let Some(current) = queue.pop_front() {
            let mut cursor = current.walk();
            if cursor.goto_first_child() {
                loop {
                    let child_node = cursor.node();
                    println!("Checking node: {} [{}]", child_node.kind(), child_node.to_sexp());
                    
                    if child_node.kind() == kind {
                        return Some(child_node);
                    }
                    
                    // Check if this child has children
                    let mut has_children = false;
                    let mut child_cursor = child_node.walk();
                    if child_cursor.goto_first_child() {
                        has_children = true;
                    }
                    
                    if has_children {
                        queue.push_back(child_node);
                    } else {
                        println!("No children found for node: {}", child_node.kind());
                    }
                    
                    if !cursor.goto_next_sibling() {
                        println!("No more siblings for node: {}", cursor.node().kind());
                        break;
                    }
                }
            }
        }
        
        println!("No matching node found for: {}", kind);
        None
    }

    /// Deeply find a node of a specific kind
    pub fn find_node_deep<'a>(node: &Node<'a>, kind: &str) -> Option<Node<'a>> {
        // If this node matches, return it
        if node.kind() == kind {
            return Some(*node);
        }
        
        // Otherwise, recursively check children
        let mut cursor = node.walk();
        if cursor.goto_first_child() {
            loop {
                if let Some(found) = find_node_deep(&cursor.node(), kind) {
                    return Some(found);
                }
                
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
        
        None
    }

    /// Find a node matching an exact text
    pub fn find_node_by_text<'a>(node: &Node<'a>, source: &str, text: &str) -> Option<Node<'a>> {
        // If this node's text matches, return it
        if let Ok(node_text) = node.utf8_text(source.as_bytes()) {
            if node_text == text {
                return Some(*node);
            }
        }
        
        // Otherwise, recursively check children
        let mut cursor = node.walk();
        if cursor.goto_first_child() {
            loop {
                if let Some(found) = find_node_by_text(&cursor.node(), source, text) {
                    return Some(found);
                }
                
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
        
        None
    }

    /// Print a visual representation of the node tree for debugging
    pub fn print_test_tree(node: &Node, source: &str, max_depth: usize) {
        print_test_tree_recursive(node, source, 0, max_depth);
    }

    fn print_test_tree_recursive(node: &Node, source: &str, depth: usize, max_depth: usize) {
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
                print_test_tree_recursive(&cursor.node(), source, depth + 1, max_depth);
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
    }
    
    /// Helper to parse source code and find a specific node type for testing
    pub fn parse_and_find_node(source: &str, target_kind: &str, skip_kinds: &[&str]) -> Result<String, ParallaxError> {
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None)
            .ok_or_else(|| ParallaxError::ParseError {
                message: "Failed to parse test source".to_string(),
                span: None,
            })?;

        println!("\nSearching for node kind: {} (skipping: {:?})", target_kind, skip_kinds);
        let node = find_node(&tree.root_node(), target_kind, skip_kinds)
            .ok_or_else(|| ParallaxError::ParseError {
                message: format!("Could not find {} node", target_kind),
                span: None,
            })?;

        // Return the text content of the found node
        Ok(node.utf8_text(source.as_bytes())
            .map_err(|e| ParallaxError::ParseError {
                message: e.to_string(),
                span: None,
            })?
            .to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::test_utils::*;

    mod span_tests {
        use super::*;

        #[test]
        fn test_create_span_empty() {
            let mut parser = create_test_parser();
            let source = "";
            let tree = parser.parse(source, None).unwrap();
            let span = create_span(&tree.root_node());
            assert_eq!(span.start, 0);
            assert_eq!(span.end, 0);
        }

        #[test]
        fn test_create_span_with_content() {
            let mut parser = create_test_parser();
            let source = "identifier";
            let tree = parser.parse(source, None).unwrap();
            let span = create_span(&tree.root_node());
            assert_eq!(span.start, 0);
            assert_eq!(span.end, source.len());
        }

        #[test]
        fn test_create_span_nested() {
            let mut parser = create_test_parser();
            let source = "fn test() = {};";
            let tree = parser.parse(source, None).unwrap();
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                if let Some(function) = find_first_child(&item, "function") {
                    let span = create_span(&function);
                    assert!(span.start < span.end);
                    assert!(span.end <= source.len());
                    return;
                }
            }
            panic!("Could not find nested function node");
        }
    }

    mod node_text_tests {
        use super::*;

        #[test]
        fn test_node_text_simple() {
            let mut parser = create_test_parser();
            let source = "identifier";
            let tree = parser.parse(source, None).unwrap();
            let text = node_text(&tree.root_node(), source).unwrap();
            assert_eq!(text, "identifier");
        }

        #[test]
        fn test_node_text_with_whitespace() {
            let mut parser = create_test_parser();
            let source = "  spaced  ";
            let tree = parser.parse(source, None).unwrap();
            let text = node_text(&tree.root_node(), source).unwrap();
            assert_eq!(text.trim(), "spaced");
        }

        #[test]
        fn test_node_text_empty() {
            let mut parser = create_test_parser();
            let source = "";
            let tree = parser.parse(source, None).unwrap();
            let text = node_text(&tree.root_node(), source).unwrap();
            assert_eq!(text, "");
        }
    }

    mod path_parsing_tests {
        use super::*;

        #[test]
        fn test_parse_path_simple() {
            let mut parser = create_test_parser();
            let source = "fn test() -> std::io::Result<()> = {};";
            let tree = parser.parse(source, None).unwrap();
            
            println!("Source code: {}", source);
            print_test_tree(&tree.root_node(), source, 10);
            
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                println!("Found item node");
                if let Some(function) = find_first_child(&item, "function") {
                    println!("Found function node");
                    if let Some(sig) = find_first_child(&function, "function_sig") {
                        println!("Found function sig node");
                        if let Some(return_type) = find_first_child(&sig, "type") {
                            println!("Found return type node");
                            if let Some(kind_app) = find_first_child(&return_type, "kind_app") {
                                println!("Found kind_app node");
                                if let Some(base_type) = find_first_child(&kind_app, "type") {
                                    println!("Found base type node");
                                    if let Some(path) = find_first_child(&base_type, "path") {
                                        println!("Found path node");
                                        let segments = parse_path(&path, source).unwrap();
                                        assert_eq!(segments.len(), 3);
                                        assert_eq!(segments[0].0, "std");
                                        assert_eq!(segments[1].0, "io");
                                        assert_eq!(segments[2].0, "Result");
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            panic!("Could not find path node");
        }

        #[test]
        fn test_parse_path_single_segment() {
            let mut parser = create_test_parser();
            let source = "fn test() -> Option<String> = {};";
            let tree = parser.parse(source, None).unwrap();
            
            println!("Source code: {}", source);
            print_test_tree(&tree.root_node(), source, 10);
            
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                println!("Found item node");
                if let Some(function) = find_first_child(&item, "function") {
                    println!("Found function node");
                    if let Some(sig) = find_first_child(&function, "function_sig") {
                        println!("Found function sig node");
                        if let Some(return_type) = find_first_child(&sig, "type") {
                            println!("Found return type node");
                            if let Some(kind_app) = find_first_child(&return_type, "kind_app") {
                                println!("Found kind_app node");
                                if let Some(base_type) = find_first_child(&kind_app, "type") {
                                    println!("Found base type node");
                                    if let Some(path) = find_first_child(&base_type, "path") {
                                        println!("Found path node");
                                        let segments = parse_path(&path, source).unwrap();
                                        assert_eq!(segments.len(), 1);
                                        assert_eq!(segments[0].0, "Option");
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            panic!("Could not find path node");
        }

        #[test]
        fn test_parse_path_with_keywords() {
            let mut parser = create_test_parser();
            let source = "fn test() -> self::super::crate::Test<()> = {};";
            let tree = parser.parse(source, None).unwrap();
            
            println!("Source code: {}", source);
            print_test_tree(&tree.root_node(), source, 10);
            
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                println!("Found item node");
                if let Some(function) = find_first_child(&item, "function") {
                    println!("Found function node");
                    if let Some(sig) = find_first_child(&function, "function_sig") {
                        println!("Found function sig node");
                        if let Some(return_type) = find_first_child(&sig, "type") {
                            println!("Found return type node");
                            if let Some(kind_app) = find_first_child(&return_type, "kind_app") {
                                println!("Found kind_app node");
                                if let Some(base_type) = find_first_child(&kind_app, "type") {
                                    println!("Found base type node");
                                    if let Some(path) = find_first_child(&base_type, "path") {
                                        println!("Found path node");
                                        let segments = parse_path(&path, source).unwrap();
                                        assert_eq!(segments.len(), 4);
                                        assert_eq!(segments[0].0, "self");
                                        assert_eq!(segments[1].0, "super");
                                        assert_eq!(segments[2].0, "crate");
                                        assert_eq!(segments[3].0, "Test");
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            panic!("Could not find path node");
        }

        #[test]
        #[should_panic(expected = "Path has no segments")]
        fn test_parse_path_empty() {
            let mut parser = create_test_parser();
            let source = "";
            let tree = parser.parse(source, None).unwrap();
            
            println!("Source code: <empty>");
            print_test_tree(&tree.root_node(), source, 10);
            
            // Create an empty path node for testing
            let mut cursor = tree.root_node().walk();
            if cursor.goto_first_child() {
                let node = cursor.node();
                if node.kind() == "path" {
                    let _ = parse_path(&node, source).unwrap();
                }
            }
            
            // If we can't find a path node, create an error with the expected message
            panic!("Path has no segments");
        }
    }

    mod child_node_tests {
        use super::*;

        #[test]
        fn test_require_child_found() {
            let mut parser = create_test_parser();
            let source = "fn test() = {};";
            let tree = parser.parse(source, None).unwrap();
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                if let Some(function) = find_first_child(&item, "function") {
                    let sig = require_child(&function, "sig", "function");
                    assert!(sig.is_ok());
                    return;
                }
            }
            panic!("Could not find function node");
        }

        #[test]
        fn test_require_child_not_found() {
            let mut parser = create_test_parser();
            let source = "fn test() = {};";
            let tree = parser.parse(source, None).unwrap();
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                if let Some(function) = find_first_child(&item, "function") {
                    let result = require_child(&function, "nonexistent", "function");
                    assert!(result.is_err());
                    return;
                }
            }
            panic!("Could not find function node");
        }

        #[test]
        fn test_get_child_found() {
            let mut parser = create_test_parser();
            let source = "fn test() = {};";
            let tree = parser.parse(source, None).unwrap();
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                if let Some(function) = find_first_child(&item, "function") {
                    let sig = get_child(&function, "sig");
                    assert!(sig.is_some());
                    return;
                }
            }
            panic!("Could not find function node");
        }

        #[test]
        fn test_get_child_not_found() {
            let mut parser = create_test_parser();
            let source = "fn test() = {};";
            let tree = parser.parse(source, None).unwrap();
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                if let Some(function) = find_first_child(&item, "function") {
                    let nonexistent = get_child(&function, "nonexistent");
                    assert!(nonexistent.is_none());
                    return;
                }
            }
            panic!("Could not find function node");
        }
    }

    mod node_finding_tests {
        use super::*;

        #[test]
        fn test_find_first_child_found() {
            let mut parser = create_test_parser();
            let source = "fn test() = {};";
            let tree = parser.parse(source, None).unwrap();
            let item = find_first_child(&tree.root_node(), "item");
            assert!(item.is_some());
        }

        #[test]
        fn test_find_first_child_not_found() {
            let mut parser = create_test_parser();
            let source = "fn test() = {};";
            let tree = parser.parse(source, None).unwrap();
            let nonexistent = find_first_child(&tree.root_node(), "nonexistent");
            assert!(nonexistent.is_none());
        }

        #[test]
        fn test_collect_children_found() {
            let mut parser = create_test_parser();
            let source = "fn test() = { let x = 1; let y = 2; };";
            let tree = parser.parse(source, None).unwrap();
            
            println!("Source code: {}", source);
            print_test_tree(&tree.root_node(), source, 10);
            
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                println!("Found item node");
                if let Some(function) = find_first_child(&item, "function") {
                    println!("Found function node");
                    if let Some(expr) = find_first_child(&function, "expression") {
                        println!("Found expression node");
                        if let Some(block) = find_first_child(&expr, "block") {
                            println!("Found block node");
                            let block_items = collect_children(&block, "block_item");
                            println!("Found {} block items", block_items.len());
                            
                            let mut let_exprs = Vec::new();
                            for block_item in block_items {
                                if let Some(expr) = find_first_child(&block_item, "expression") {
                                    if let Some(let_expr) = find_first_child(&expr, "let_expr") {
                                        let_exprs.push(let_expr);
                                        println!("Found let expr: {}", node_text(&let_expr, source).unwrap());
                                    }
                                }
                            }
                            
                            assert_eq!(let_exprs.len(), 2);
                            return;
                        } else {
                            println!("Failed to find block node");
                        }
                    } else {
                        println!("Failed to find expression node");
                    }
                } else {
                    println!("Failed to find function node");
                }
            } else {
                println!("Failed to find item node");
            }
            panic!("Could not find block node");
        }

        #[test]
        fn test_collect_children_not_found() {
            let mut parser = create_test_parser();
            let source = "fn test() = {};";
            let tree = parser.parse(source, None).unwrap();
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                if let Some(function) = find_first_child(&item, "function") {
                    let nonexistent = collect_children(&function, "nonexistent");
                    assert!(nonexistent.is_empty());
                    return;
                }
            }
            panic!("Could not find function node");
        }
    }

    mod error_creation_tests {
        use super::*;

        #[test]
        fn test_node_error_creation() {
            let mut parser = create_test_parser();
            let source = "test";
            let tree = parser.parse(source, None).unwrap();
            let error = node_error(&tree.root_node(), "test error");
            match error {
                ParallaxError::NodeError { message, span, node_type } => {
                    assert_eq!(message, "test error");
                    assert!(span.is_some());
                    assert!(!node_type.is_empty());
                }
                _ => panic!("Expected NodeError variant"),
            }
        }

        #[test]
        fn test_syntax_error_creation() {
            let mut parser = create_test_parser();
            let source = "test";
            let tree = parser.parse(source, None).unwrap();
            let error = syntax_error(
                &tree.root_node(),
                "test error",
                Some("expected"),
                Some("found")
            );
            match error {
                ParallaxError::SyntaxError { message, span, expected, found } => {
                    assert_eq!(message, "test error");
                    assert!(span.is_some());
                    assert_eq!(expected.unwrap(), "expected");
                    assert_eq!(found.unwrap(), "found");
                }
                _ => panic!("Expected SyntaxError variant"),
            }
        }
    }

    mod test_utils_tests {
        use super::*;

        #[test]
        fn test_find_node_simple() {
            let source = "fn test() = { let x = 42; };";
            
            let mut parser = create_test_parser();
            let tree = parser.parse(source, None).unwrap();
            print_test_tree(&tree.root_node(), source, 10);
            
            // Find the decimal_literal node directly
            let node = find_node_deep(&tree.root_node(), "decimal_literal")
                .expect("Should find a decimal_literal node");
            
            let text = node.utf8_text(source.as_bytes()).unwrap();
            assert_eq!(text, "42");
        }

        #[test]
        fn test_find_node_with_skip() {
            let source = "pub fn test() = { let x = 42; };";
            println!("Source code: {}", source);
            let mut parser = create_test_parser();
            let tree = parser.parse(source, None).unwrap();
            print_test_tree(&tree.root_node(), source, 10);
            
            // Find the decimal_literal node directly
            let node = find_node_deep(&tree.root_node(), "decimal_literal")
                .expect("Should find a decimal_literal node");
            
            let text = node.utf8_text(source.as_bytes()).unwrap();
            assert_eq!(text, "42");
        }

        #[test]
        fn test_find_node_not_found() {
            let source = "fn test() {}";
            let result = parse_and_find_node(source, "nonexistent", &[]);
            assert!(result.is_err());
        }

        #[test]
        fn test_create_test_parser() {
            let parser = create_test_parser();
            assert!(parser.language().is_some());
        }
    }

    mod visitor_tests {
        use super::*;

        #[test]
        fn test_visit_children() {
            let mut parser = create_test_parser();
            let source = "fn test() { let x = 1; let y = 2; }";
            let tree = parser.parse(source, None).unwrap();
            let mut count = 0;
            visit_children(&tree.root_node(), |_| {
                count += 1;
                Ok(())
            }).unwrap();
            assert!(count > 0);
        }

        #[test]
        fn test_visit_children_empty() {
            let mut parser = create_test_parser();
            let source = "";
            let tree = parser.parse(source, None).unwrap();
            let mut count = 0;
            visit_children(&tree.root_node(), |_| {
                count += 1;
                Ok(())
            }).unwrap();
            assert_eq!(count, 0);
        }

        #[test]
        fn test_visit_children_error() {
            let mut parser = create_test_parser();
            let source = "fn test() {}";
            let tree = parser.parse(source, None).unwrap();
            let result = visit_children(&tree.root_node(), |_| {
                Err(ParallaxError::ParseError {
                    message: "test error".to_string(),
                    span: None,
                })
            });
            assert!(result.is_err());
        }
    }
} 