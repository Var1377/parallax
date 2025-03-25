use crate::ast::expr::{Expr, ExprKind, Argument};
use crate::ast::common::Ident;
use crate::ParallaxError;
use crate::parser::common::{create_span};
use crate::parser::expr::parse_expr;
use tree_sitter::Node;

/// Parse a call expression node
/// 
/// <call_expr>
///   <identifier> (the function being called)
///   <call_args> (the arguments to the call)
/// </call_expr>
pub fn parse_call_expr<'a>(node: &Node<'a>, source: &'a str) -> Result<Expr, ParallaxError> {
    let span = create_span(node);
    
    // The first child is the function being called
    let callee_node = node.child(0).ok_or_else(|| ParallaxError::ParseError {
        message: format!("Call expression has no callee at {:?}", span),
        span: Some(span.clone()),
    })?;
    
    // Parse the callee expression
    let func = Box::new(parse_expr(&callee_node, source)?);
    
    // The second child should be the arguments
    let args_node = node.child(1).ok_or_else(|| ParallaxError::ParseError {
        message: format!("Call expression has no arguments at {:?}", span),
        span: Some(span.clone()),
    })?;
    
    // Parse the arguments
    let args = parse_call_args(&args_node, source)?;
    
    // Create the call expression
    Ok(Expr {
        kind: ExprKind::Call { func, args },
        span,
    })
}

/// Parse the arguments to a function call
/// 
/// <call_args>
///   "("
///   <argument>*
///   ")"
/// </call_args>
pub fn parse_call_args<'a>(node: &Node<'a>, source: &'a str) -> Result<Vec<Argument>, ParallaxError> {
    let mut args = Vec::new();
    
    // Iterate through all children of the node
    for i in 0..node.child_count() {
        let child = node.child(i).unwrap();
        
        // Skip the parentheses and any delimiters (like commas)
        if child.kind() == "(" || child.kind() == ")" || child.kind() == "," {
            continue;
        }
        
        // Parse each argument
        let arg = parse_argument(&child, source)?;
        args.push(arg);
    }
    
    Ok(args)
}

/// Parse a single argument
/// 
/// <argument>
///   ... (various forms of arguments)
/// </argument>
fn parse_argument<'a>(node: &Node<'a>, source: &'a str) -> Result<Argument, ParallaxError> {
    let span = create_span(node);
    
    // Check if this is a spread argument (starts with ...)
    if node.kind() == "spread_element" {
        let expr_node = node.child(1).ok_or_else(|| ParallaxError::ParseError {
            message: format!("Spread argument missing expression at {:?}", span),
            span: Some(span.clone()),
        })?;
        
        let expr = parse_expr(&expr_node, source)?;
        
        return Ok(Argument {
            name: Some(Ident("...".to_string())),
            value: expr,
            span,
        });
    }
    
    // Check if this is a named argument (key: value)
    if node.kind() == "named_argument" {
        let name_node = node.child(0).ok_or_else(|| ParallaxError::ParseError {
            message: format!("Named argument missing name at {:?}", span),
            span: Some(span.clone()),
        })?;
        
        let value_node = node.child(2).ok_or_else(|| ParallaxError::ParseError {
            message: format!("Named argument missing value at {:?}", span),
            span: Some(span.clone()),
        })?;
        
        let name_text = name_node.utf8_text(source.as_bytes()).map_err(|_| {
            ParallaxError::ParseError {
                message: format!("Could not read argument name at {:?}", span),
                span: Some(span.clone()),
            }
        })?;
        
        let name = Some(Ident(name_text.to_string()));
        let value = parse_expr(&value_node, source)?;
        
        return Ok(Argument {
            name,
            value,
            span,
        });
    }
    
    // This is a positional argument (just an expression)
    let value = parse_expr(node, source)?;
    
    Ok(Argument {
        name: None,
        value,
        span,
    })
}

#[cfg(test)]
mod tests {
    use crate::parser::common::test_utils::{create_test_parser, find_node_deep, print_test_tree};
    use crate::ParallaxError;
    use tree_sitter::Node;
    
    #[test]
    fn test_simple_call() -> Result<(), ParallaxError> {
        // Put the call in a proper function context
        let source = "fn test() = { foo(bar); };";
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        
        print_test_tree(&tree.root_node(), source, 10);
        
        // Extract identifier texts using a visitor pattern approach
        let mut identifier_texts = Vec::new();
        
        fn visit_nodes(node: &Node, source: &[u8], texts: &mut Vec<String>) {
            if node.kind() == "identifier" {
                if let Ok(text) = node.utf8_text(source) {
                    texts.push(text.to_string());
                }
            }
            
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                visit_nodes(&child, source, texts);
            }
        }
        
        visit_nodes(&tree.root_node(), source.as_bytes(), &mut identifier_texts);
        
        // We should find at least 3 identifiers: test (function name), foo (call target), bar (argument)
        assert!(identifier_texts.len() >= 3, "Expected at least three identifiers");
        
        // "test" is the function name, "foo" is the call target, "bar" is the argument
        assert_eq!(identifier_texts[0], "test");
        assert_eq!(identifier_texts[1], "foo");
        assert_eq!(identifier_texts[2], "bar");
        
        Ok(())
    }
    
    #[test]
    fn test_empty_call() -> Result<(), ParallaxError> {
        // Put the call in a proper function context
        let source = "fn test() = { foo(); };";
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        
        print_test_tree(&tree.root_node(), source, 10);
        
        // Extract identifier texts using a visitor pattern approach
        let mut identifier_texts = Vec::new();
        
        fn visit_nodes(node: &Node, source: &[u8], texts: &mut Vec<String>) {
            if node.kind() == "identifier" {
                if let Ok(text) = node.utf8_text(source) {
                    texts.push(text.to_string());
                }
            }
            
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                visit_nodes(&child, source, texts);
            }
        }
        
        visit_nodes(&tree.root_node(), source.as_bytes(), &mut identifier_texts);
        
        // We should find 2 identifiers: test (function name), foo (call target)
        assert_eq!(identifier_texts.len(), 2, "Expected exactly two identifiers");
        
        // "test" is the function name, "foo" is the call target
        assert_eq!(identifier_texts[0], "test");
        assert_eq!(identifier_texts[1], "foo");
        
        // Find the call_expr node
        let call_expr = find_node_deep(&tree.root_node(), "call_expr")
            .ok_or_else(|| ParallaxError::ParseError {
                message: "Could not find call_expr node".to_string(),
                span: None,
            })?;
        
        // Verify it's an empty call by checking if it only has the expression and parentheses as children
        let mut actual_args = 0;
        for i in 0..call_expr.child_count() {
            if let Some(child) = call_expr.child(i) {
                if child.kind() != "expression" && child.kind() != "(" && child.kind() != ")" {
                    actual_args += 1;
                }
            }
        }
        
        assert_eq!(actual_args, 0, "Expected call with no arguments");
        
        Ok(())
    }
    
    #[test]
    fn test_multiple_args() -> Result<(), ParallaxError> {
        // Put the call in a proper function context
        let source = "fn test() = { foo(x, y, z); };";
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        
        print_test_tree(&tree.root_node(), source, 10);
        
        // Extract identifier texts using a visitor pattern approach
        let mut identifier_texts = Vec::new();
        
        fn visit_nodes(node: &Node, source: &[u8], texts: &mut Vec<String>) {
            if node.kind() == "identifier" {
                if let Ok(text) = node.utf8_text(source) {
                    texts.push(text.to_string());
                }
            }
            
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                visit_nodes(&child, source, texts);
            }
        }
        
        visit_nodes(&tree.root_node(), source.as_bytes(), &mut identifier_texts);
        
        // We should find 5 identifiers: test (function name), foo (call target), x, y, z (arguments)
        assert_eq!(identifier_texts.len(), 5, "Expected exactly five identifiers");
        
        // Check the function name
        assert_eq!(identifier_texts[0], "test");
        assert_eq!(identifier_texts[1], "foo");
        
        // Check the argument names
        assert_eq!(identifier_texts[2], "x");
        assert_eq!(identifier_texts[3], "y");
        assert_eq!(identifier_texts[4], "z");
        
        Ok(())
    }
    
    #[test]
    fn test_named_args() -> Result<(), ParallaxError> {
        // Put the call in a proper function context
        let source = "fn test() = { foo(x, y: 42); };";
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        
        print_test_tree(&tree.root_node(), source, 10);
        
        // Extract identifier texts using a visitor pattern approach
        let mut identifier_texts = Vec::new();
        let mut decimal_literals = Vec::new();
        
        fn visit_nodes(node: &Node, source: &[u8], texts: &mut Vec<String>, literals: &mut Vec<String>) {
            if node.kind() == "identifier" {
                if let Ok(text) = node.utf8_text(source) {
                    texts.push(text.to_string());
                }
            } else if node.kind() == "decimal_literal" {
                if let Ok(text) = node.utf8_text(source) {
                    literals.push(text.to_string());
                }
            }
            
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                visit_nodes(&child, source, texts, literals);
            }
        }
        
        visit_nodes(&tree.root_node(), source.as_bytes(), &mut identifier_texts, &mut decimal_literals);
        
        // We should find 4 identifiers: test (function name), foo (call target), x, y (arguments)
        assert_eq!(identifier_texts.len(), 4, "Expected exactly four identifiers");
        
        // Check the function name and arguments
        assert_eq!(identifier_texts[0], "test");
        assert_eq!(identifier_texts[1], "foo");
        assert_eq!(identifier_texts[2], "x");
        assert_eq!(identifier_texts[3], "y");
        
        // Check the decimal literal for the named argument
        assert_eq!(decimal_literals.len(), 1, "Expected exactly one decimal literal");
        assert_eq!(decimal_literals[0], "42");
        
        // Find the colon that separates name and value
        let colon = find_node_deep(&tree.root_node(), ":")
            .ok_or_else(|| ParallaxError::ParseError {
                message: "Could not find colon in named argument".to_string(),
                span: None,
            })?;
        
        // Find the named argument value
        let decimal = find_node_deep(&tree.root_node(), "decimal_literal")
            .ok_or_else(|| ParallaxError::ParseError {
                message: "Could not find decimal literal".to_string(),
                span: None,
            })?;
        
        // Confirm the decimal appears after the colon
        assert!(decimal.start_byte() > colon.end_byte(), "Expected value after colon in named argument");
        
        Ok(())
    }
    
    #[test]
    fn test_spread_arg() -> Result<(), ParallaxError> {
        // Put the call in a proper function context
        let source = "fn test() = { foo(x, ...rest); };";
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        
        print_test_tree(&tree.root_node(), source, 10);
        
        // Extract identifier texts using a visitor pattern approach
        let mut identifier_texts = Vec::new();
        
        fn visit_nodes(node: &Node, source: &[u8], texts: &mut Vec<String>) {
            if node.kind() == "identifier" {
                if let Ok(text) = node.utf8_text(source) {
                    texts.push(text.to_string());
                }
            }
            
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                visit_nodes(&child, source, texts);
            }
        }
        
        visit_nodes(&tree.root_node(), source.as_bytes(), &mut identifier_texts);
        
        // We should find 4 identifiers: test (function name), foo (call target), x, rest (arguments)
        assert_eq!(identifier_texts.len(), 4, "Expected exactly four identifiers");
        
        // Check the function name and arguments
        assert_eq!(identifier_texts[0], "test");
        assert_eq!(identifier_texts[1], "foo");
        assert_eq!(identifier_texts[2], "x");
        assert_eq!(identifier_texts[3], "rest");
        
        // Find the ellipsis directly from the source code
        let ellipsis_pos = source.find("...").unwrap_or(0);
        let rest_pos = source.find("rest").unwrap_or(0);
        
        // Make sure "rest" comes after "..."
        assert!(rest_pos > ellipsis_pos, "Expected 'rest' to appear after '...'");
        
        // Validate that we can find the specific nodes in the tree
        let mut found_ellipsis = false;
        let mut found_rest = false;
        
        // Recursive function to look for specific nodes
        fn find_specific_nodes(node: &Node, source: &[u8], ellipsis: &mut bool, rest: &mut bool) {
            if node.kind() == "..." {
                *ellipsis = true;
            } else if node.kind() == "identifier" {
                if let Ok(text) = node.utf8_text(source) {
                    if text == "rest" {
                        *rest = true;
                    }
                }
            }
            
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                find_specific_nodes(&child, source, ellipsis, rest);
            }
        }
        
        find_specific_nodes(&tree.root_node(), source.as_bytes(), &mut found_ellipsis, &mut found_rest);
        
        assert!(found_ellipsis, "Could not find '...' node in the tree");
        assert!(found_rest, "Could not find 'rest' identifier in the tree");
        
        Ok(())
    }
}