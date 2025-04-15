use tree_sitter::Node;
use crate::error::SyntaxError;
use crate::ast::common::Ident;
use crate::ast::items::{StructField, StructDef};
use super::common;
use super::types::parse_type;

/// Parse a struct body (e.g., { x: i32, y: bool })
pub fn parse_struct_body(node: &Node, source: &str) -> Result<Vec<StructField>, SyntaxError> {
    let mut fields = Vec::new();
    let mut cursor = node.walk();
    
    if cursor.goto_first_child() {
        // Skip the opening brace
        if !cursor.goto_next_sibling() {
            return Ok(fields); // Empty struct body
        }
        
        // Process each field until we hit the closing brace
        loop {
            let current = cursor.node();
            if current.kind() == "struct_field" {
                fields.push(parse_struct_field(&current, source)?);
            }
            
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    
    Ok(fields)
}

/// Parse a field in a struct (e.g., x: i32)
fn parse_struct_field(node: &Node, source: &str) -> Result<StructField, SyntaxError> {
    let span = common::create_span(node);
    
    // Check for visibility
    let visibility = common::find_first_child(node, "visibility").is_some();
    
    // Get field name and type
    let name_node = common::require_child(node, "name", "struct_field")?;
    let name = Ident {
        name: common::node_text(&name_node, source)?,
        span: common::create_span(&name_node),
    };
    
    let type_node = common::require_child(node, "type", "struct_field")?;
    let ty = parse_type(&type_node, source)?;
    
    Ok(StructField {
        name,
        ty,
        visibility,
        span,
    })
}

/// Parse a struct definition (e.g., struct Point { x: i32, y: i32 })
pub fn parse_struct_def(node: &Node, source: &str) -> Result<StructDef, SyntaxError> {
    let span = common::create_span(node);
    
    // Get the struct name
    let name_node = common::require_child(node, "name", "struct")?;
    let name = Ident {
        name: common::node_text(&name_node, source)?,
        span: common::create_span(&name_node),
    };
    
    // Parse generic parameters if present
    let generic_params = super::items::parse_generic_params(node, source)?;
    
    // Parse where clause if present
    let where_clause = super::items::parse_where_clause(node, source)?;
    
    // Parse struct body
    let body_node = common::require_child(node, "body", "struct")?;
    let fields = parse_struct_body(&body_node, source)?;
    
    Ok(StructDef {
        name,
        generic_params,
        where_clause,
        fields,
        span,
    })
}

/// Parse a tuple body (e.g., (i32, bool))
pub fn parse_tuple_body(node: &Node, source: &str) -> Result<Vec<crate::ast::types::Type>, SyntaxError> {
    let mut types = Vec::new();
    let mut cursor = node.walk();
    
    if cursor.goto_first_child() {
        // Skip the opening parenthesis
        if !cursor.goto_next_sibling() {
            return Ok(types); // Empty tuple body
        }
        
        // Process each type until we hit the closing parenthesis
        loop {
            let current = cursor.node();
            if current.kind() == "type" {
                types.push(parse_type(&current, source)?);
            }
            
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    
    Ok(types)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::common::test_utils::*;
    
    #[test]
    fn test_empty_struct_body() -> Result<(), SyntaxError> {
        let source = "struct Empty { }";
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        
        let body_node = find_node(&tree.root_node(), "struct_body", &[])
            .ok_or_else(|| SyntaxError::ParseError {
                message: "Could not find struct_body node".to_string(),
                span: None,
            })?;
        
        let fields = parse_struct_body(&body_node, source)?;
        assert_eq!(fields.len(), 0);
        
        Ok(())
    }
    
    #[test]
    fn test_struct_body_with_fields() -> Result<(), SyntaxError> {
        let source = "struct Point { x: i32, y: i32 }";
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        
        let body_node = find_node(&tree.root_node(), "struct_body", &[])
            .ok_or_else(|| SyntaxError::ParseError {
                message: "Could not find struct_body node".to_string(),
                span: None,
            })?;
        
        let fields = parse_struct_body(&body_node, source)?;
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name.name, "x");
        assert_eq!(fields[1].name.name, "y");
        
        Ok(())
    }
    
    #[test]
    fn test_struct_body_with_visibility() -> Result<(), SyntaxError> {
        let source = "struct User { name: String, pub age: i32 }";
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        
        let body_node = find_node(&tree.root_node(), "struct_body", &[])
            .ok_or_else(|| SyntaxError::ParseError {
                message: "Could not find struct_body node".to_string(),
                span: None,
            })?;
        
        let fields = parse_struct_body(&body_node, source)?;
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name.name, "name");
        assert!(!fields[0].visibility);
        assert_eq!(fields[1].name.name, "age");
        assert!(fields[1].visibility);
        
        Ok(())
    }
} 