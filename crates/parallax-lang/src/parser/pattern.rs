use tree_sitter::Node;
use crate::error::ParallaxError;
use crate::ast::*;
use crate::ast::common::Ident;
use super::expr;
use super::common;

pub(crate) fn parse_pattern(node: &Node, source: &str) -> Result<Pattern, ParallaxError> {
    let span = common::create_span(node);
    println!("Parsing pattern node of kind: {}", node.kind());

    let kind = match node.kind() {
        "pattern" => {
            // Get the first child which should be the actual pattern
            let pattern_node = node.child(0)
                .ok_or_else(|| common::node_error(node, "Pattern node has no children"))?;
            println!("Found pattern child of kind: {}", pattern_node.kind());
            return parse_pattern(&pattern_node, source);
        },
        "identifier" => {
            let text = common::node_text(node, source)?;
            println!("Found identifier pattern: {}", text);
            PatternKind::Identifier(Ident(text))
        },
        "literal" => {
            println!("Found literal pattern");
            let literal_node = node.child(0)
                .ok_or_else(|| common::node_error(node, "Literal pattern has no children"))?;
            println!("Literal child kind: {}", literal_node.kind());
            PatternKind::Literal(expr::parse_literal(&literal_node, source)?)
        },
        "tuple_pattern" => {
            println!("Found tuple pattern");
            let mut patterns = Vec::new();
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                // Skip opening parenthesis
                if cursor.goto_next_sibling() {
                    loop {
                        let current = cursor.node();
                        if current.kind() == "pattern" {
                            patterns.push(parse_pattern(&current, source)?);
                        }
                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }
                }
            }
            PatternKind::Tuple(patterns)
        },
        "array_pattern" => {
            println!("Found array pattern");
            let mut patterns = Vec::new();
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                // Skip opening bracket
                if cursor.goto_next_sibling() {
                    loop {
                        let current = cursor.node();
                        if current.kind() == "pattern" {
                            patterns.push(parse_pattern(&current, source)?);
                        }
                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }
                }
            }
            PatternKind::Array(patterns)
        },
        "struct_pattern" => {
            println!("Found struct pattern");
            let path = common::require_child(node, "path", "struct_pattern")
                .and_then(|n| common::parse_path(&n, source))?;

            let mut fields = Vec::new();
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                // Skip path and opening brace
                if cursor.goto_next_sibling() && cursor.goto_next_sibling() {
                    loop {
                        let current = cursor.node();
                        if current.kind() == "pattern_field" {
                            let name = if let Some(name_node) = current.child_by_field_name("name") {
                                Ident(common::node_text(&name_node, source)?)
                            } else {
                                continue;
                            };
                            let pattern = if let Some(pattern_node) = current.child_by_field_name("pattern") {
                                Some(parse_pattern(&pattern_node, source)?)
                            } else {
                                None
                            };
                            fields.push(PatternField {
                                name,
                                pattern,
                                span: common::create_span(&current),
                            });
                        }
                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }
                }
            }
            PatternKind::Struct { path, fields }
        },
        "constructor" => {
            println!("Found constructor pattern");
            let path = common::require_child(node, "path", "constructor")
                .and_then(|n| common::parse_path(&n, source))?;
            
            let args_node = common::require_child(node, "args", "constructor")?;
            let args = Box::new(parse_pattern(&args_node, source)?);
            
            PatternKind::Constructor { path, args }
        },
        "rest_pattern" => {
            println!("Found rest pattern");
            PatternKind::Rest
        },
        "or_pattern" => {
            println!("Found or pattern");
            let mut cursor = node.walk();
            if !cursor.goto_first_child() {
                return Err(common::node_error(node, "Or pattern has no children"));
            }

            let left = Box::new(parse_pattern(&cursor.node(), source)?);
            
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(node, "Or pattern missing right side"));
            }
            
            let right = Box::new(parse_pattern(&cursor.node(), source)?);
            
            PatternKind::Or(left, right)
        },
        "wildcard_pattern" => {
            println!("Found wildcard pattern");
            PatternKind::Wildcard
        },
        _ => {
            println!("Unknown pattern kind: {}", node.kind());
            return Err(common::node_error(node, &format!("Unknown pattern kind: {}", node.kind())));
        }
    };

    Ok(Pattern::new(kind, span))
}

fn parse_pattern_fields(node: &Node, source: &str) -> Result<Vec<PatternField>, ParallaxError> {
    let mut fields = Vec::new();
    common::visit_children(node, |child| {
        if child.kind() == "pattern_field" {
            fields.push(parse_pattern_field(child, source)?);
        }
        Ok(())
    })?;
    Ok(fields)
}

fn parse_pattern_field(node: &Node, source: &str) -> Result<PatternField, ParallaxError> {
    let span = common::create_span(node);

    let name = common::find_first_child(node, "identifier")
        .ok_or_else(|| common::node_error(node, "Pattern field missing name"))?;

    let pattern = common::find_first_child(node, "pattern")
        .map(|n| parse_pattern(&n, source))
        .transpose()?;

    let name_text = common::node_text(&name, source)?;
    Ok(PatternField {
        name: Ident(name_text),
        pattern,
        span,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::common::test_utils::*;
    use crate::ast::pattern::PatternKind;
    use crate::ast::common::Literal;

    fn test_pattern_node(source: &str) -> Result<Pattern, ParallaxError> {
        let source_file = format!("fn main() = match x {{ {source} => (), }}");
        let mut parser = create_test_parser();
        let tree = parser.parse(&source_file, None).unwrap();
        
        println!("\nParsing source: {}", source);
        println!("Parse tree:");
        common::test_utils::print_test_tree(&tree.root_node(), source, 10);
        
        // First try to find a direct pattern node
        let pattern_node = find_node(&tree.root_node(), "pattern", &[])
            .or_else(|| {
                // If not found, try to find a match arm and get its pattern
                find_node(&tree.root_node(), "match_arm", &[])
                    .and_then(|arm| find_node(&arm, "pattern", &[]))
            })
            .ok_or_else(|| ParallaxError::ParseError {
                message: "Could not find pattern node".to_string(),
                span: None,
            })?;
        
        println!("Found pattern node: {}", pattern_node.kind());
        parse_pattern(&pattern_node, source)
    }

    #[test]
    fn test_literal_patterns() -> Result<(), ParallaxError> {
        // Test integer literal pattern
        let pat = test_pattern_node("42")?;
        match pat.kind {
            PatternKind::Literal(Literal::Int(n)) => assert_eq!(n, 42),
            _ => panic!("Expected integer literal pattern"),
        }

        // Test string literal pattern
        let pat = test_pattern_node("match x { \"hello\" => y }")?;
        match pat.kind {
            PatternKind::Literal(Literal::String(s)) => assert_eq!(s, "hello"),
            _ => panic!("Expected string literal pattern"),
        }

        // Test boolean literal pattern
        let pat = test_pattern_node("match x { true => y }")?;
        match pat.kind {
            PatternKind::Literal(Literal::Bool(b)) => assert!(b),
            _ => panic!("Expected boolean literal pattern"),
        }

        Ok(())
    }

    #[test]
    fn test_identifier_pattern() -> Result<(), ParallaxError> {
        let pat = test_pattern_node("let x = 42;")?;
        match pat.kind {
            PatternKind::Identifier(ident) => assert_eq!(ident.0, "x"),
            _ => panic!("Expected identifier pattern"),
        }
        Ok(())
    }

    #[test]
    fn test_tuple_pattern() -> Result<(), ParallaxError> {
        let pat = test_pattern_node("let (x, y, z) = point;")?;
        match pat.kind {
            PatternKind::Tuple(patterns) => {
                assert_eq!(patterns.len(), 3);
                match &patterns[0].kind {
                    PatternKind::Identifier(ident) => assert_eq!(ident.0, "x"),
                    _ => panic!("Expected identifier pattern"),
                }
            },
            _ => panic!("Expected tuple pattern"),
        }
        Ok(())
    }

    #[test]
    fn test_struct_pattern() -> Result<(), ParallaxError> {
        let pat = test_pattern_node("match p { Point { x, y } => z }")?;
        match pat.kind {
            PatternKind::Struct { path, fields } => {
                assert_eq!(path[0].0, "Point");
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name.0, "x");
                assert_eq!(fields[1].name.0, "y");
            },
            _ => panic!("Expected struct pattern"),
        }
        Ok(())
    }

    #[test]
    fn test_wildcard_pattern() -> Result<(), ParallaxError> {
        let pat = test_pattern_node("match x { _ => y }")?;
        assert!(matches!(pat.kind, PatternKind::Wildcard));
        Ok(())
    }

    #[test]
    fn test_rest_pattern() -> Result<(), ParallaxError> {
        let pat = test_pattern_node("let Point { x, .. } = p;")?;
        match pat.kind {
            PatternKind::Struct { path, fields } => {
                assert_eq!(path[0].0, "Point");
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].name.0, "x");
                // Rest pattern is handled implicitly in struct patterns
            },
            _ => panic!("Expected struct pattern with rest"),
        }
        Ok(())
    }

    #[test]
    fn test_or_pattern() -> Result<(), ParallaxError> {
        let pat = test_pattern_node("match x { 1 | 2 => y }")?;
        match pat.kind {
            PatternKind::Or(left, right) => {
                match left.kind {
                    PatternKind::Literal(Literal::Int(n)) => assert_eq!(n, 1),
                    _ => panic!("Expected integer literal pattern"),
                }
                match right.kind {
                    PatternKind::Literal(Literal::Int(n)) => assert_eq!(n, 2),
                    _ => panic!("Expected integer literal pattern"),
                }
            },
            _ => panic!("Expected or pattern"),
        }
        Ok(())
    }

    #[test]
    fn test_constructor_pattern() -> Result<(), ParallaxError> {
        let pat = test_pattern_node("match x { Some(y) => z }")?;
        match pat.kind {
            PatternKind::Constructor { path, args } => {
                assert_eq!(path[0].0, "Some");
                match args.kind {
                    PatternKind::Identifier(ident) => assert_eq!(ident.0, "y"),
                    _ => panic!("Expected identifier pattern"),
                }
            },
            _ => panic!("Expected constructor pattern"),
        }
        Ok(())
    }
}