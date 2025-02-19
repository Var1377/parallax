use tree_sitter::Node;
use crate::error::ParallaxError;
use crate::ast::*;
use crate::ast::common::{Span, Ident};
use crate::ast::expr::{UnaryOp, BinaryOp, Argument, GenericParam, Kind};
use crate::ast::pattern::Pattern;
use crate::ast::items::Parameter;
use super::types::parse_type;
use super::pattern;
use super::common;
use crate::visitor::Visitor;

pub(crate) fn parse_literal(node: &Node, source: &str) -> Result<Literal, ParallaxError> {
    let text = common::node_text(node, source)?;
    match node.kind() {
        "integer_literal" => {
            // Get the first child which will be decimal_literal, hex_literal, etc.
            let literal_node = node.child(0)
                .ok_or_else(|| common::node_error(node, "Integer literal has no children"))?;
            let text = common::node_text(&literal_node, source)?;
            match literal_node.kind() {
                "decimal_literal" => Ok(Literal::Int(text.parse().expect("Failed to parse decimal literal"))),
                "hex_literal" => Ok(Literal::Int(i64::from_str_radix(text.trim_start_matches("0x"), 16).expect("Failed to parse hex literal"))),
                "octal_literal" => Ok(Literal::Int(i64::from_str_radix(text.trim_start_matches("0o"), 8).expect("Failed to parse octal literal"))),
                "binary_literal" => Ok(Literal::Int(i64::from_str_radix(text.trim_start_matches("0b"), 2).expect("Failed to parse binary literal"))),
                _ => Err(common::node_error(&literal_node, &format!("Unknown integer literal type: {}", literal_node.kind()))),
            }
        },
        "float_literal" => Ok(Literal::Float(text.parse().expect("Failed to parse float literal"))),
        "string_literal" => Ok(Literal::String(text.trim_matches('"').to_string())),
        "character_literal" => {
            let content = text.trim_matches('\'').chars().next()
                .ok_or_else(|| common::node_error(node, "Empty character literal"))?;
            Ok(Literal::Char(content))
        },
        "boolean_literal" => Ok(Literal::Bool(text == "true")),
        _ => Err(common::node_error(node, &format!("Unknown literal type: {}", node.kind()))),
    }
}

struct ExpressionFinder<'a> {
    source: &'a str,
    found_expr: Option<Expr>,
}

impl<'a> ExpressionFinder<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            found_expr: None,
        }
    }
}

impl<'a> Visitor for ExpressionFinder<'a> {
    type Error = ParallaxError;

    fn visit_expr(&mut self, expr: &Expr) -> Result<(), Self::Error> {
        // Store the first expression we find
        if self.found_expr.is_none() {
            self.found_expr = Some(expr.clone());
        }
        Ok(())
    }
}

pub fn parse_expr(node: &Node, source: &str) -> Result<Expr, ParallaxError> {
    let span = common::create_span(node);

    // Get the first child if this is a generic expression node
    let node = if node.kind() == "expression" {
        node.child(0).ok_or_else(|| common::node_error(node, "Expression node has no children"))?
    } else {
        node.clone()
    };

    let kind = match node.kind() {
        "literal" => {
            let literal_node = node.child(0).ok_or_else(|| common::node_error(&node, "Literal node has no children"))?;
            ExprKind::Literal(parse_literal(&literal_node, source)?)
        },
        "path" => {
            let mut segments = Vec::new();
            common::visit_children(&node, |current| {
                    if current.kind() == "identifier" {
                    segments.push(Ident(common::node_text(&current, source)?));
                }
                Ok(())
            })?;
            ExprKind::Path(segments)
        },
        "binary_expr" => {
            let left = common::require_child(&node, "left", "binary_expr")
                .and_then(|n| parse_expr(&n, source))?;
            
            let op_node = common::require_child(&node, "op", "binary_expr")?;
            let op = parse_binary_op(&op_node, source)?;

            let right = common::require_child(&node, "right", "binary_expr")
                .and_then(|n| parse_expr(&n, source))?;

            ExprKind::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }
        },
        "unary_expr" => {
            let op_node = common::require_child(&node, "op", "unary_expr")?;
            let op_text = common::node_text(&op_node, source)?;
            
            let op = match op_text.as_str() {
                "-" => UnaryOp::Neg,
                "!" => UnaryOp::Not,
                "&" => UnaryOp::Ref,
                "*" => UnaryOp::Deref,
                _ => return Err(common::node_error(&op_node, &format!("Unknown unary operator: {}", op_text))),
            };

            let expr = common::require_child(&node, "expr", "unary_expr")
                .and_then(|n| parse_expr(&n, source))?;

            ExprKind::Unary {
                op,
                expr: Box::new(expr),
            }
        },
        "call_expr" => {
            let func = if let Some(expr_node) = node.child(0) {
                Box::new(parse_expr(&expr_node, source)?)
            } else {
                return Err(common::node_error(&node, "Call expression has no function"));
            };
            
            let mut args = Vec::new();
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                // Skip function expression
                if cursor.goto_next_sibling() {
                    // Skip opening parenthesis
                    if cursor.goto_next_sibling() {
                loop {
                            let current = cursor.node();
                            if current.kind() == "argument" {
                                args.push(parse_argument(&current, source)?);
                            } else if current.kind() == "expression" {
                                args.push(Argument {
                                    name: None,
                                    value: parse_expr(&current, source)?,
                                    span: common::create_span(&current),
                                });
                    }
                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
            }
            }
            }

            ExprKind::Call { func, args }
        },
        "field_access" => {
            let object = common::require_child(&node, "object", "field_access")
                .and_then(|n| parse_expr(&n, source))?;

            let field = common::require_child(&node, "field", "field_access")
                .and_then(|n| common::node_text(&n, source))
                .map(Ident)?;

            ExprKind::Field {
                object: Box::new(object),
                name: field,
            }
        },
        "array_expr" => {
            let mut elements = Vec::new();
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                // Skip the opening bracket
                if cursor.goto_next_sibling() {
                    loop {
                        let current = cursor.node();
                        if current.kind() == "expression" {
                            elements.push(parse_expr(&current, source)?);
                        }
                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }
                }
            }
            ExprKind::Array(elements)
        },
        "tuple_expr" => {
            let mut elements = Vec::new();
            common::visit_children(&node, |child| {
                    if child.kind() == "expression" {
                        elements.push(parse_expr(&child, source)?);
                    }
                Ok(())
            })?;
            ExprKind::Tuple(elements)
        },
        "map_expr" => {
            let mut entries = Vec::new();
            let mut cursor = node.walk();
            
            // Only traverse children if we have any
            if cursor.goto_first_child() {
                // Skip '#' and '{' tokens
                if cursor.goto_next_sibling() && cursor.goto_next_sibling() {
                    // Process entries until we hit the closing brace
                    while cursor.node().kind() == "map_entry" {
                        let current = cursor.node();
                        
                        // Get first expression as key
                        let mut entry_cursor = current.walk();
                        if !entry_cursor.goto_first_child() {
                            if !cursor.goto_next_sibling() {
                                break;
                            }
                            continue;
                        }
                        let key = parse_expr(&entry_cursor.node(), source)?;
                        
                        // Skip colon
                        if !entry_cursor.goto_next_sibling() || entry_cursor.node().kind() != ":" {
                            if !cursor.goto_next_sibling() {
                                break;
                            }
                            continue;
                        }
                        
                        // Get value expression
                        if !entry_cursor.goto_next_sibling() {
                            if !cursor.goto_next_sibling() {
                                break;
                            }
                            continue;
                        }
                        let value = parse_expr(&entry_cursor.node(), source)?;
                        
                        entries.push((key, value));
                        
                        // Move to next entry or break if we're done
                        if !cursor.goto_next_sibling() {
                            break;
                        }
                        // Skip comma if present
                        if cursor.node().kind() == "," {
                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }
                    }
                }
            }
            ExprKind::Map(entries)
        },
        "hashset_expr" => {
            let mut elements = Vec::new();
            common::visit_children(&node, |child| {
                if child.kind() == "expression" {
                    elements.push(parse_expr(&child, source)?);
                }
                Ok(())
            })?;
            ExprKind::HashSet(elements)
        },
        "let_expr" => {
            println!("Parsing let expression");
            let mut cursor = node.walk();
            if !cursor.goto_first_child() {
                return Err(common::node_error(&node, "Let expression has no children"));
            }

            // Skip 'let' keyword
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "Let expression has no pattern"));
            }

            // Parse pattern
            let pattern = pattern::parse_pattern(&cursor.node(), source)?;
            println!("Parsed pattern");

            // Check for type annotation
            let mut type_ann = None;
            if cursor.goto_next_sibling() {
                if cursor.node().kind() == ":" {
                    println!("Found type annotation");
                    if cursor.goto_next_sibling() {
                        type_ann = Some(parse_type(&cursor.node(), source)?);
                        cursor.goto_next_sibling(); // Move to '=' or next token
                    }
                }
            }

            // Check for '='
            if cursor.node().kind() != "=" {
                return Err(common::node_error(&node, "Let expression missing '='"));
            }

            // Parse value
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "Let expression has no value"));
            }
            println!("Parsing value: {}", cursor.node().kind());
            let value = Box::new(parse_expr(&cursor.node(), source)?);

            ExprKind::Let {
                pattern,
                type_ann,
                value,
            }
        },
        "struct_expr" => {
            let path = if let Some(path_node) = node.child(0) {
                let mut segments = Vec::new();
                common::visit_children(&path_node, |child| {
                    if child.kind() == "identifier" {
                        segments.push(Ident(common::node_text(&child, source)?));
                    }
                    Ok(())
                })?;
                segments
            } else {
                return Err(common::node_error(&node, "Struct expression has no path"));
            };

            let mut fields = Vec::new();
            let mut base = None;

            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                // Skip path
                if cursor.goto_next_sibling() {
                    // Skip opening brace
                    if cursor.goto_next_sibling() {
                        loop {
                            let current = cursor.node();
                            match current.kind() {
                                "field_init" => {
                                    let mut field_cursor = current.walk();
                                    if !field_cursor.goto_first_child() {
                                        continue;
                                    }

                                    let name = Ident(common::node_text(&field_cursor.node(), source)?);

                                    // Skip ':'
                                    if !field_cursor.goto_next_sibling() || field_cursor.node().kind() != ":" {
                                        continue;
                                    }

                                    // Skip ':'
                                    if !field_cursor.goto_next_sibling() {
                                        continue;
                                    }

                                    let value = parse_expr(&field_cursor.node(), source)?;
                                    fields.push((name, value));
                                },
                                "base" => {
                                    if let Some(expr_node) = current.child_by_field_name("expression") {
                                        base = Some(Box::new(parse_expr(&expr_node, source)?));
                                    }
                                },
                                "}" => break,
                                _ => {}
                            }
                            if !cursor.goto_next_sibling() {
                                break;
                            }
                        }
                    }
                }
            }

            ExprKind::Struct { path, fields, base }
        },
        "paren_expr" => {
            let inner = node.child(1).ok_or_else(|| common::node_error(&node, "Parenthesized expression is empty"))?;
            ExprKind::Paren(Box::new(parse_expr(&inner, source)?))
        },
        "if_expr" => {
            let mut cursor = node.walk();
            if !cursor.goto_first_child() {
                return Err(common::node_error(&node, "If expression has no children"));
            }

            // Skip 'if' keyword
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "If expression has no condition"));
            }

            // Parse condition
            let condition = Box::new(parse_expr(&cursor.node(), source)?);

            // Skip 'then' keyword
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "If expression has no then branch"));
            }
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "If expression has no then branch"));
            }

            // Parse then branch
            let then_branch = Box::new(parse_expr(&cursor.node(), source)?);

            // Check for else branch
            let else_branch = if cursor.goto_next_sibling() && cursor.node().kind() == "else" {
                if cursor.goto_next_sibling() {
                    Some(Box::new(parse_expr(&cursor.node(), source)?))
                } else {
                    None
                }
            } else {
                None
            };

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            }
        },
        "match_expr" => {
            let mut cursor = node.walk();
            if !cursor.goto_first_child() {
                return Err(common::node_error(&node, "Match expression has no children"));
            }

            // Skip 'match' keyword
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "Match expression has no scrutinee"));
            }

            // Parse scrutinee
            let scrutinee = Box::new(parse_expr(&cursor.node(), source)?);

            // Skip opening brace
            if !cursor.goto_next_sibling() || cursor.node().kind() != "{" {
                return Err(common::node_error(&node, "Match expression missing '{'"));
            }

            let mut arms = Vec::new();
            while cursor.goto_next_sibling() {
                let current = cursor.node();
                if current.kind() == "match_arm" {
                    let mut arm_cursor = current.walk();
                    if !arm_cursor.goto_first_child() {
                        continue;
                    }

                    // Parse pattern
                    let pattern = pattern::parse_pattern(&arm_cursor.node(), source)?;

                    // Skip '=>'
                    if !arm_cursor.goto_next_sibling() {
                        continue;
                    }

                    // Parse expression
                    let expr = parse_expr(&arm_cursor.node(), source)?;
                    arms.push((pattern, expr));
                } else if current.kind() == "}" {
                    break;
                }
            }

            ExprKind::Match {
                scrutinee,
                arms,
            }
        },
        "lambda_expr" => {
            println!("Parsing lambda expression");
            let mut cursor = node.walk();
            if !cursor.goto_first_child() {
                return Err(common::node_error(&node, "Lambda expression has no children"));
            }

            // Check for generic parameters
            let generic_params = if cursor.node().kind() == "generic_params" {
                println!("Found generic parameters");
                let params = parse_generic_params(&cursor.node(), source)?;
                cursor.goto_next_sibling();
                Some(params)
            } else {
                None
            };

            // Skip initial '|'
            if cursor.node().kind() != "|" {
                return Err(common::node_error(&node, "Lambda expression missing opening '|'"));
            }
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "Lambda expression has no parameters"));
            }

            // Parse parameters
            let mut params = Vec::new();
            if cursor.node().kind() == "parameters" {
                println!("Found parameters node");
                let mut param_cursor = cursor.node().walk();
                if param_cursor.goto_first_child() {
                    loop {
                        let current = param_cursor.node();
                        println!("Processing parameter node: {}", current.kind());
                        if current.kind() == "parameter" {
                            params.push(parse_parameter(&current, source)?);
                        }
                        if !param_cursor.goto_next_sibling() {
                            break;
                        }
                    }
                }
                cursor.goto_next_sibling();
            }

            // Skip closing '|'
            if cursor.node().kind() != "|" {
                return Err(common::node_error(&node, "Lambda expression missing closing '|'"));
            }
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "Lambda expression missing '=>'"));
            }

            // Skip '=>'
            if cursor.node().kind() != "=>" {
                return Err(common::node_error(&node, "Lambda expression missing '=>'"));
            }
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(&node, "Lambda expression has no body"));
            }

            // Parse body
            println!("Parsing lambda body: {}", cursor.node().kind());
            let body = Box::new(parse_expr(&cursor.node(), source)?);

            ExprKind::Lambda {
                generic_params,
                params,
                body,
            }
        },
        "block" => {
            println!("Parsing block expression");
            let mut stmts = Vec::new();
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                println!("Found first child: {}", cursor.node().kind());
                // Skip opening brace
                if cursor.goto_next_sibling() {
                    println!("After skipping brace: {}", cursor.node().kind());
                loop {
            let current = cursor.node();
                        println!("Processing node: {}", current.kind());
                        match current.kind() {
                            "expression" => {
                                println!("Found expression in block");
                                stmts.push(parse_expr(&current, source)?);
                            },
                            "block_item" => {
                                println!("Found block item");
                                if let Some(expr_node) = current.child(0) {
                                    println!("Block item contains: {}", expr_node.kind());
                                    stmts.push(parse_expr(&expr_node, source)?);
                                }
                            },
                            "}" => break,
                            _ => println!("Skipping node: {}", current.kind())
                    }
                    if !cursor.goto_next_sibling() {
                            println!("No more siblings");
                        break;
                    }
                }
            }
            }
            println!("Block has {} statements", stmts.len());
            ExprKind::Block(stmts)
        },
        _ => return Err(common::node_error(&node, &format!("Unknown expression kind: {}", node.kind()))),
    };

    Ok(Expr::new(kind, span))
}

fn parse_binary_op(node: &Node, source: &str) -> Result<BinaryOp, ParallaxError> {
    let text = common::node_text(node, source)?;
    match text.as_str() {
        "+" => Ok(BinaryOp::Add),
        "-" => Ok(BinaryOp::Sub),
        "*" => Ok(BinaryOp::Mul),
        "/" => Ok(BinaryOp::Div),
        "==" => Ok(BinaryOp::Eq),
        "<" => Ok(BinaryOp::Lt),
        ">" => Ok(BinaryOp::Gt),
        "&&" => Ok(BinaryOp::And),
        "||" => Ok(BinaryOp::Or),
        "->" => Ok(BinaryOp::Arrow),
        op => Err(common::node_error(node, &format!("Unknown binary operator: {}", op))),
    }
}

fn parse_arguments(node: &Node, source: &str) -> Result<Vec<Argument>, ParallaxError> {
    let mut args = Vec::new();
    common::visit_children(node, |arg_node| {
            if arg_node.kind() == "argument" {
                args.push(parse_argument(&arg_node, source)?);
            }
        Ok(())
    })?;
    Ok(args)
}

fn parse_argument(node: &Node, source: &str) -> Result<Argument, ParallaxError> {
    let span = common::create_span(node);

    // Check for positional argument
    if let Some(expr) = common::get_child(node, "positional") {
        return Ok(Argument {
            name: None,
            value: parse_expr(&expr, source)?,
            span,
        });
    }

    // Check for named argument
    if let Some(name) = common::get_child(node, "name") {
        let value = common::require_child(node, "value", "argument")
            .and_then(|n| parse_expr(&n, source))?;
        
        return Ok(Argument {
            name: Some(Ident(common::node_text(&name, source)?)),
            value,
            span,
        });
    }

    // Check for spread argument
    if let Some(expr) = common::get_child(node, "spread") {
        return Ok(Argument {
            name: None,
            value: parse_expr(&expr, source)?,
            span,
        });
    }

    Err(common::node_error(node, "Invalid argument type"))
}

fn parse_generic_params(node: &Node, source: &str) -> Result<Vec<GenericParam>, ParallaxError> {
    let mut params = Vec::new();
    common::visit_children(node, |param| {
        if param.kind() == "generic_param" {
            let is_phantom = param.child(0)
                .map_or(false, |n| n.kind() == "phantom");
            
            let name = common::require_child(&param, "name", "generic_param")
                .and_then(|n| common::node_text(&n, source))
                .map(Ident)?;

            let kind = common::get_child(&param, "kind")
                .map(|n| parse_kind(&n, source))
                .transpose()?;

            params.push(GenericParam {
                is_phantom,
                name,
                kind,
            });
        }
        Ok(())
    })?;
    Ok(params)
}

fn parse_kind(node: &Node, source: &str) -> Result<Kind, ParallaxError> {
    match node.kind() {
        "star" => Ok(Kind::Star),
        "function_kind" => {
            let param = common::require_child(node, "param", "function_kind")
                .and_then(|n| parse_kind(&n, source))?;
            let ret = common::require_child(node, "return", "function_kind")
                .and_then(|n| parse_kind(&n, source))?;
            Ok(Kind::Function(Box::new(param), Box::new(ret)))
        },
        "tuple_kind" => {
            let mut kinds = Vec::new();
            common::visit_children(node, |child| {
                if child.kind() == "kind" {
                        kinds.push(parse_kind(&child, source)?);
                    }
                Ok(())
            })?;
            Ok(Kind::Tuple(kinds))
        },
        _ => Err(common::node_error(node, &format!("Unknown kind: {}", node.kind()))),
    }
}

fn parse_parameter(node: &Node, source: &str) -> Result<Parameter, ParallaxError> {
    let span = common::create_span(node);

    let pattern = common::require_child(node, "pattern", "parameter")
        .and_then(|n| pattern::parse_pattern(&n, source))?;

    let ty = if let Some(type_node) = node.child_by_field_name("type") {
        Some(parse_type(&type_node, source)?)
    } else {
        None
    };

    let default_value = common::get_child(node, "value")
        .map(|n| parse_expr(&n, source))
        .transpose()?;

    let is_variadic = node.child(0)
        .map_or(false, |n| n.kind() == "...");

    Ok(Parameter {
        pattern,
        ty,
        default_value,
        is_variadic,
        span,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::common::Literal, parser::print_tree_structure};

    // Helper function to find the expression node in a function
    fn find_expression_node<'a>(node: &Node<'a>) -> Option<Node<'a>> {
        // First child should be an item
        let mut cursor = node.walk();
        if !cursor.goto_first_child() {
            return None;
        }
        
        let item_node = cursor.node();
        
        // Find the function node within the item
        let mut item_cursor = item_node.walk();
        if !item_cursor.goto_first_child() {
            return None;
        }
        
        // Skip visibility if present
        let mut current = item_cursor.node();
        if current.kind() == "visibility" {
            if !item_cursor.goto_next_sibling() {
                return None;
            }
            current = item_cursor.node();
        }
        
        if current.kind() != "function" {
            return None;
        }
        
        // Navigate through function children to find expression
        let mut fn_cursor = current.walk();
        if !fn_cursor.goto_first_child() {
            return None;
        }
        
        // Skip function_sig and equals sign
        loop {
            current = fn_cursor.node();
            
            if current.kind() == "expression" {
                // For lambda expressions, we need to find the lambda_expr node
                let mut expr_cursor = current.walk();
                if expr_cursor.goto_first_child() {
                    let first_child = expr_cursor.node();
                    if first_child.kind() == "binary_expr" {
                        // Check if the left side is a lambda_expr
                        let mut bin_cursor = first_child.walk();
                        if bin_cursor.goto_first_child() {
                            let left = bin_cursor.node();
                            if left.kind() == "expression" {
                                let mut left_cursor = left.walk();
                                if left_cursor.goto_first_child() {
                                    let lambda = left_cursor.node();
                                    if lambda.kind() == "lambda_expr" {
                                        return Some(lambda);
                                    }
                                }
                            }
                        }
                    } else if first_child.kind() == "lambda_expr" {
                        return Some(first_child);
                    }
                }
                return Some(current);
            }
            
            if !fn_cursor.goto_next_sibling() {
                break;
            }
        }
        None
    }

    fn test_expr_node(source: &str) -> Result<Expr, ParallaxError> {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        let tree = parser.parse(source, None).unwrap();
        print_tree_structure(&tree.root_node(), source, 10);
        let expr_node = find_expression_node(&tree.root_node()).expect("Could not find expression node");
        parse_expr(&expr_node, source)
    }

    #[test]
    fn test_literal_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test integer literal
        let source = "fn test() = 42;";
        println!("\nTesting integer literal:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Int(42))));

        // Test float literal
        let source = "fn test() = 3.14;";
        println!("\nTesting float literal:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Float(3.14))));

        // Test string literal
        let source = "fn test() = \"hello\";";
        println!("\nTesting string literal:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Literal(Literal::String(s)) = &expr.kind {
            assert_eq!(s, "hello");
        } else {
            panic!("Expected string literal");
        }

        // Test character literal
        let source = "fn test() = 'a';";
        println!("\nTesting character literal:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Char('a'))));

        // Test boolean literals
        let source = "fn test() = true;";
        println!("\nTesting boolean literal (true):");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Bool(true))));


        let source = "fn test() = false;";
        println!("\nTesting boolean literal (false):");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Bool(false))));

        Ok(())
    }

    #[test]
    fn test_binary_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test addition
        let source = "fn test() = 1 + 2;";
        println!("\nTesting binary addition:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Add, .. }));

        // Test subtraction
        let source = "fn test() = 3 - 4;";
        println!("\nTesting binary subtraction:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Sub, .. }));

        // Test multiplication
        let source = "fn test() = 5 * 6;";
        println!("\nTesting binary multiplication:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Mul, .. }));

        // Test division
        let source = "fn test() = 8 / 2;";
        println!("\nTesting binary division:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Div, .. }));

        // Test equality comparison
        let source = "fn test() = x == y;";
        println!("\nTesting binary equality:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Eq, .. }));

        // Test less than comparison
        let source = "fn test() = a < b;";
        println!("\nTesting binary less than:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Lt, .. }));

        // Test greater than comparison
        let source = "fn test() = c > d;";
        println!("\nTesting binary greater than:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Gt, .. }));

        // Test logical AND
        let source = "fn test() = true && false;";
        println!("\nTesting binary logical AND:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::And, .. }));

        // Test logical OR
        let source = "fn test() = x || y;";
        println!("\nTesting binary logical OR:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Or, .. }));

        // Test arrow operator
        let source = "fn test() = a -> b;";
        println!("\nTesting binary arrow operator:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Arrow, .. }));

        Ok(())
    }

    #[test]
    fn test_call_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test simple function call
        let source = "fn test() = foo();";
        println!("\nTesting simple function call:");
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Call { args, .. } if args.is_empty()));

        // Test function call with arguments
        let source = "fn test() = add(1, 2);";
        println!("\nTesting function call with arguments:");
        let expr = test_expr_node(source)?;
        println!("Expr: {:?}", expr);
        assert!(matches!(expr.kind, ExprKind::Call { args, .. } if args.len() == 2));

        // Test nested function calls
        let source = "fn test() = foo(bar(1), baz(2));";
        println!("\nTesting nested function calls:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Call { args, .. } = expr.kind {
            assert_eq!(args.len(), 2);
            assert!(matches!(args[0].value.kind, ExprKind::Call { .. }));
            assert!(matches!(args[1].value.kind, ExprKind::Call { .. }));
        } else {
            panic!("Expected call expression");
        }

        // Test named arguments
        let source = "fn test() = foo(x: 1, y: 2);";
        println!("\nTesting function call with named arguments:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Call { args, .. } = expr.kind {
            assert_eq!(args.len(), 2);
            assert!(args[0].name.is_some());
            assert!(args[1].name.is_some());
        } else {
            panic!("Expected call expression");
        }

        Ok(())
    }

    #[test]
    fn test_unary_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test negation
        let source = "fn test() = -42;";
        println!("\nTesting unary negation:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Unary { op: UnaryOp::Neg, .. }));

        // Test logical not
        let source = "fn test() = !true;";
        println!("\nTesting unary logical not:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Unary { op: UnaryOp::Not, .. }));

        // Test reference
        let source = "fn test() = &x;";
        println!("\nTesting unary reference:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Unary { op: UnaryOp::Ref, .. }));

        // Test dereference
        let source = "fn test() = *ptr;";
        println!("\nTesting unary dereference:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Unary { op: UnaryOp::Deref, .. }));

        Ok(())
    }

    #[test]
    fn test_field_access_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test simple field access
        let source = "fn test() = point.x;";
        println!("\nTesting simple field access:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Field { .. }));

        // Test chained field access
        let source = "fn test() = person.address.city;";
        println!("\nTesting chained field access:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Field { object, .. } = expr.kind {
            assert!(matches!(object.kind, ExprKind::Field { .. }));
        } else {
            panic!("Expected field access expression");
        }

        // Test tuple field access
        let source = "fn test() = tuple.0;";
        println!("\nTesting tuple field access:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Field { .. }));

        Ok(())
    }

    #[test]
    fn test_array_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test empty array
        let source = "fn test() = [];";
        println!("\nTesting empty array:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Array(elements) if elements.is_empty()));

        // Test array with elements
        let source = "fn test() = [1, 2, 3];";
        println!("\nTesting array with elements:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Array(elements) if elements.len() == 3));

        // Test array with expressions
        let source = "fn test() = [x + 1, y * 2, z()];";
        println!("\nTesting array with expressions:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Array(elements) if elements.len() == 3));

        Ok(())
    }

    #[test]
    fn test_tuple_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test empty tuple
        let source = "fn test() = ();";
        println!("\nTesting empty tuple:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Tuple(elements) if elements.is_empty()));

        // Test single element tuple
        let source = "fn test() = (1,);";
        println!("\nTesting single element tuple:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Tuple(elements) if elements.len() == 1));

        // Test multiple element tuple
        let source = "fn test() = (1, true, \"hello\");";
        println!("\nTesting multiple element tuple:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Tuple(elements) if elements.len() == 3));

        // Test nested tuples
        let source = "fn test() = (1, (2, 3), 4);";
        println!("\nTesting nested tuples:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Tuple(elements) = expr.kind {
            assert_eq!(elements.len(), 3);
            assert!(matches!(elements[1].kind, ExprKind::Tuple(_)));
        } else {
            panic!("Expected tuple expression");
        }

        Ok(())
    }

    #[test]
    fn test_let_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test simple let
        let source = "fn test() = let x = 42;";
        println!("\nTesting simple let expression:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Let { .. }));

        // Test let with type annotation
        let source = "fn test() = let x: i32 = 42;";
        println!("\nTesting let expression with type annotation:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Let { type_ann, .. } = expr.kind {
            assert!(type_ann.is_some());
        } else {
            panic!("Expected let expression");
        }

        // Test let with pattern
        let source = "fn test() = let (x, y) = point;";
        println!("\nTesting let expression with pattern:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Let { pattern, .. } = expr.kind {
            assert!(matches!(pattern.kind, PatternKind::Tuple(_)));
        } else {
            panic!("Expected let expression");
        }

        Ok(())
    }

    #[test]
    fn test_struct_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test empty struct
        let source = "fn test() = Point {};";
        println!("\nTesting empty struct:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Struct { fields, .. } if fields.is_empty()));

        // Test struct with fields
        let source = "fn test() = Point { x: 1, y: 2 };";
        println!("\nTesting struct with fields:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Struct { fields, .. } if fields.len() == 2));

        // Test struct with base
        let source = "fn test() = Point { x: 1, ..base };";
        println!("\nTesting struct with base:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Struct { fields, base, .. } = expr.kind {
            assert_eq!(fields.len(), 1);
            assert!(base.is_some());
        } else {
            panic!("Expected struct expression");
        }

        Ok(())
    }

    #[test]
    fn test_block_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test empty block
        let source = "fn test() = {};";
        println!("\nTesting empty block:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Block(stmts) if stmts.is_empty()));

        // Test block with single expression
        let source = "fn test() = { 42 };";
        println!("\nTesting block with single expression:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Block(stmts) if stmts.len() == 1));

        // Test block with multiple statements
        let source = "fn test() = { let x = 1; let y = 2; x + y };";
        println!("\nTesting block with multiple statements:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Block(stmts) if stmts.len() == 3));

        Ok(())
    }

    #[test]
    fn test_if_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test if-then
        let source = "fn test() = if x then y;";
        println!("\nTesting if-then expression:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::If { else_branch, .. } if else_branch.is_none()));

        // Test if-then-else
        let source = "fn test() = if x then y else z;";
        println!("\nTesting if-then-else expression:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::If { else_branch, .. } if else_branch.is_some()));

        // Test nested if
        let source = "fn test() = if x then if y then a else b else c;";
        println!("\nTesting nested if expression:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::If { then_branch, .. } = expr.kind {
            assert!(matches!(then_branch.kind, ExprKind::If { .. }));
        } else {
            panic!("Expected if expression");
        }

        Ok(())
    }

    #[test]
    fn test_match_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test simple match
        let source = "fn test() = match x { 1 => a, 2 => b, _ => c };";
        println!("\nTesting simple match expression:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Match { arms, .. } = expr.kind {
            assert_eq!(arms.len(), 3);
        } else {
            panic!("Expected match expression");
        }

        // Test match with patterns
        let source = "fn test() = match opt { Some(x) => x, None => 0 };";
        println!("\nTesting match with patterns:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Match { arms, .. } = expr.kind {
            assert_eq!(arms.len(), 2);
        } else {
            panic!("Expected match expression");
        }

        Ok(())
    }

    #[test]
    fn test_lambda_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test simple lambda
        let source = "fn test() = |x| => x + 1;";
        println!("\nTesting simple lambda:");
        println!("Source: {}", source);
        let tree = parser.parse(source, None).unwrap();
        print_tree_structure(&tree.root_node(), source, 10);
        let expr_node = find_expression_node(&tree.root_node()).expect("Could not find expression node");
        println!("Found expression node: {}", expr_node.kind());
        let expr = parse_expr(&expr_node, source)?;
        println!("Parsed expression: {:?}", expr);
        if let ExprKind::Lambda { params, generic_params, .. } = &expr.kind {
            assert_eq!(params.len(), 1);
            assert!(generic_params.is_none());
        } else {
            panic!("Expected lambda expression, got {:?}", expr.kind);
        }

        // Test lambda with type annotations
        let source = "fn test() = |x: i32| => x + 1;";
        println!("\nTesting lambda with type annotations:");
        println!("Source: {}", source);
        let tree = parser.parse(source, None).unwrap();
        print_tree_structure(&tree.root_node(), source, 10);
        let expr_node = find_expression_node(&tree.root_node()).expect("Could not find expression node");
        println!("Found expression node: {}", expr_node.kind());
        let expr = parse_expr(&expr_node, source)?;
        println!("Parsed expression: {:?}", expr);
        if let ExprKind::Lambda { params, .. } = &expr.kind {
            assert_eq!(params.len(), 1);
            assert!(params[0].ty.is_some());
        } else {
            panic!("Expected lambda expression, got {:?}", expr.kind);
        }

        // Test lambda with multiple parameters
        let source = "fn test() = |x, y| => x + y;";
        println!("\nTesting lambda with multiple parameters:");
        println!("Source: {}", source);
        let tree = parser.parse(source, None).unwrap();
        print_tree_structure(&tree.root_node(), source, 10);
        let expr_node = find_expression_node(&tree.root_node()).expect("Could not find expression node");
        println!("Found expression node: {}", expr_node.kind());
        let expr = parse_expr(&expr_node, source)?;
        println!("Parsed expression: {:?}", expr);
        if let ExprKind::Lambda { params, .. } = &expr.kind {
            assert_eq!(params.len(), 2);
        } else {
            panic!("Expected lambda expression, got {:?}", expr.kind);
        }

        // Test lambda with generic parameters
        let source = "fn test() = <T>|x: T| => x;";
        println!("\nTesting lambda with generic parameters:");
        println!("Source: {}", source);
        let tree = parser.parse(source, None).unwrap();
        print_tree_structure(&tree.root_node(), source, 10);
        let expr_node = find_expression_node(&tree.root_node()).expect("Could not find expression node");
        println!("Found expression node: {}", expr_node.kind());
        let expr = parse_expr(&expr_node, source)?;
        println!("Parsed expression: {:?}", expr);
        if let ExprKind::Lambda { generic_params, .. } = &expr.kind {
            assert!(generic_params.is_some());
            assert_eq!(generic_params.as_ref().unwrap().len(), 1);
        } else {
            panic!("Expected lambda expression, got {:?}", expr.kind);
        }

        Ok(())
    }

    #[test]
    fn test_map_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test empty map
        let source = "fn test() = #{}";
        println!("\nTesting empty map:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Map(entries) if entries.is_empty()));

        // Test map with entries
        let source = "fn test() = #{ \"x\": 1, \"y\": 2 }";
        println!("\nTesting map with entries:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Map(entries) = expr.kind {
            assert_eq!(entries.len(), 2);
            // Verify first entry is "x": 1
            if let ExprKind::Literal(Literal::String(key)) = &entries[0].0.kind {
                assert_eq!(key, "x");
            } else {
                panic!("Expected string literal key");
            }
            if let ExprKind::Literal(Literal::Int(value)) = &entries[0].1.kind {
                assert_eq!(*value, 1);
            } else {
                panic!("Expected integer literal value");
            }
        } else {
            panic!("Expected map expression");
        }

        // Test map with expression keys and values
        let source = "fn test() = #{ x + 1: y * 2, \"z\": foo() }";
        println!("\nTesting map with expression entries:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::Map(entries) = expr.kind {
            assert_eq!(entries.len(), 2);
            assert!(matches!(entries[0].0.kind, ExprKind::Binary { .. }));
            assert!(matches!(entries[1].1.kind, ExprKind::Call { .. }));
        } else {
            panic!("Expected map expression");
        }

        Ok(())
    }

    #[test]
    fn test_hashset_expressions() -> Result<(), ParallaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| ParallaxError::ParserInitError(e.to_string()))?;

        // Test empty hashset
        let source = "fn test() = #[]";
        println!("\nTesting empty hashset:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::HashSet(elements) if elements.is_empty()));

        // Test hashset with elements
        let source = "fn test() = #[1, 2, 3, 4, 5]";
        println!("\nTesting hashset with elements:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::HashSet(elements) = expr.kind {
            assert_eq!(elements.len(), 5);
            // Verify first element is 1
            if let ExprKind::Literal(Literal::Int(value)) = &elements[0].kind {
                assert_eq!(*value, 1);
            } else {
                panic!("Expected integer literal");
            }
        } else {
            panic!("Expected hashset expression");
        }

        // Test hashset with expressions
        let source = "fn test() = #[x + 1, y * 2, foo()]";
        println!("\nTesting hashset with expressions:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        if let ExprKind::HashSet(elements) = expr.kind {
            assert_eq!(elements.len(), 3);
            assert!(matches!(elements[0].kind, ExprKind::Binary { .. }));
            assert!(matches!(elements[2].kind, ExprKind::Call { .. }));
        } else {
            panic!("Expected hashset expression");
        }

        Ok(())
    }
}