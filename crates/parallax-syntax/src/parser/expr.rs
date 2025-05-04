use super::common;
use super::pattern;
use super::types::parse_type;
use crate::ast::common::{Ident, Literal};
use crate::ast::expr::{Argument, BinaryOp, Expr, ExprKind, GenericParam, Kind, UnaryOp};
use crate::ast::items::Parameter;
use crate::error::SyntaxError;
use crate::visitor::Visitor;
use tree_sitter::Node;
use tree_sitter::Tree;
use miette::SourceSpan;
use tree_sitter::Parser;
use crate::parser::literals;

struct ExpressionFinder<'a> {
    #[allow(dead_code)]
    source: &'a str,
    found_expr: Option<Expr>,
}

impl<'a> ExpressionFinder<'a> {
    #[allow(dead_code)]
    fn new(source: &'a str) -> Self {
        Self {
            source,
            found_expr: None,
        }
    }
}

impl<'a> Visitor for ExpressionFinder<'a> {
    type Error = SyntaxError;

    fn visit_expr(&mut self, expr: &Expr) -> Result<(), Self::Error> {
        // Store the first expression we find
        if self.found_expr.is_none() {
            self.found_expr = Some(expr.clone());
        }
        Ok(())
    }
}

/// Parse a binary expression
pub fn parse_binary_expr(node: Node, source: &str) -> Result<Expr, SyntaxError> {
    let span = common::create_span(&node);

    let left_node = common::require_child(&node, "left", "binary_expr")?;
    let right_node = common::require_child(&node, "right", "binary_expr")?;

    let left = parse_expr(&left_node, source)?;
    let right = parse_expr(&right_node, source)?;

    // Get the operator text directly from the source
    let op_node = node.child_by_field_name("operator").unwrap_or(node);

    // Extract the operator from the node text by looking at characters between left and right
    let op_text = if op_node.kind() == "binary_expr" {
        // If no explicit operator node, try to infer it from context
        // Find the first non-whitespace character between the end of left and start of right
        let left_end = left_node.end_byte();
        let right_start = right_node.start_byte();
        let between_text = &source[left_end..right_start];

        between_text.trim().to_string()
    } else {
        common::node_text(&op_node, source)?
    };

    let op = match op_text.as_str() {
        "+" => BinaryOp::Add,
        "-" => BinaryOp::Sub,
        "*" => BinaryOp::Mul,
        "/" => BinaryOp::Div,
        "==" => BinaryOp::Eq,
        "!=" => BinaryOp::Ne,
        "<" => BinaryOp::Lt,
        ">" => BinaryOp::Gt,
        "<=" => BinaryOp::Le,
        ">=" => BinaryOp::Ge,
        "&&" => BinaryOp::And,
        "||" => BinaryOp::Or,
        "->" => BinaryOp::Arrow,
        "&" => BinaryOp::BitAnd,
        "|" => BinaryOp::BitOr,
        "^" => BinaryOp::BitXor,
        "<<" => BinaryOp::Shl,
        ">>" => BinaryOp::Shr,
        "%" => BinaryOp::Rem,
        _ => {
            // If it's not a recognized operator, default to Add for now to avoid test failures
            // In a production environment, we would want to return an error or handle this better
            return Err(common::node_error(
                &node,
                &format!("Unknown binary operator: '{}'", op_text),
            ));
        }
    };

    Ok(Expr::new(
        ExprKind::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
        span,
    ))
}

/// Parse a unary expression
pub fn parse_unary_expr(node: Node, source: &str) -> Result<Expr, SyntaxError> {
    let span = common::create_span(&node);

    let op_node = common::require_child(&node, "op", "unary_expr")?;
    let expr_node = common::require_child(&node, "expr", "unary_expr")?;

    let expr = parse_expr(&expr_node, source)?;

    let op_str = common::node_text(&op_node, source)?;
    let op = match op_str.trim() {
        "-" => UnaryOp::Neg,
        "!" => UnaryOp::Not,
        _ => {
            return Err(SyntaxError::ParseError {
                message: format!("Unknown unary operator: '{}'", op_str),
                span: Some(span),
            });
        }
    };

    Ok(Expr::new(
        ExprKind::Unary {
            op,
            expr: Box::new(expr),
        },
        span,
    ))
}

pub fn parse_expr(node: &Node, source: &str) -> Result<Expr, SyntaxError> {
    let span = common::create_span(node);

    // Get the first child if this is a generic expression node
    let node = if node.kind() == "expression" {
        node.child(0)
            .ok_or_else(|| common::node_error(node, "Expression node has no children"))?
    } else {
        node.clone()
    };

    // Special case for parenthesized expressions
    if node.kind() == "(" {
        // Find the inner expression node by looking for the expression child
        // We can't use next_sibling because parenthesis is a token, not a node with children
        // Instead, we need to get the parent node which contains the full construct
        let parent = node.parent().ok_or_else(|| {
            common::node_error(&node, "Parenthesized expression has no parent node")
        })?;

        // Look for the expression after the opening parenthesis
        let mut inner_expr = None;
        let mut cursor = parent.walk();

        // Find the inner expression by scanning all children
        if cursor.goto_first_child() {
            // Skip nodes until we find our opening parenthesis
            let mut found_paren = false;

            loop {
                let current = cursor.node();

                if found_paren && current.kind() == "expression" {
                    // We found the expression after the opening parenthesis
                    inner_expr = Some(current);
                    break;
                }

                if current.id() == node.id() {
                    // We found our opening parenthesis
                    found_paren = true;
                }

                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }

        if let Some(inner) = inner_expr {
            // Parse the inner expression and wrap it in a parenthesized expression
            let inner_parsed = parse_expr(&inner, source)?;
            return Ok(Expr::new(ExprKind::Paren(Box::new(inner_parsed)), span));
        } else {
            return Err(common::node_error(
                &node,
                "Could not find inner expression for parenthesized expression",
            ));
        }
    }

    println!("[parse_expr] Processing node kind: '{}', text: '{}'", node.kind(), common::node_text(&node, source).unwrap_or("<error>".to_string()));
    let kind = match node.kind() {
        "literal" => {
            let literal_node = node
                .child(0)
                .ok_or_else(|| common::node_error(&node, "Literal node has no children"))?;
            ExprKind::Literal(literals::parse_literal(&literal_node, source)?)
        }
        "identifier" => {
            // Handle a single identifier node directly as a path
            let ident = Ident {
                name: common::node_text(&node, source)?,
                span: common::create_span(&node),
            };
            ExprKind::Path(vec![ident])
        }
        "path" => {
            let mut segments = Vec::new();
            common::visit_children(&node, |current| {
                // Handle different kinds of path segments based on the grammar
                let kind = current.kind();
                if kind == "identifier" || kind == "self" || kind == "super" || kind == "crate" {
                    segments.push(Ident {
                        name: common::node_text(&current, source)?, // Use node_text for all segment kinds
                        span: common::create_span(&current),
                    });
                } 
                // Skip over '::' tokens
                else if kind != "::" {
                     // Report error for unexpected node kind within path
                     return Err(common::node_error(&current, &format!("Unexpected node kind '{}' in path", kind)));
                }
                Ok(())
            })?;
            if segments.is_empty() {
                 return Err(common::node_error(&node, "Path expression resulted in zero segments"));
            }
            ExprKind::Path(segments)
        }
        "binary_expr" => return parse_binary_expr(node.clone(), source),
        "unary_expr" => return parse_unary_expr(node.clone(), source),
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
                            println!("[parse_expr->call_expr->args_loop] Processing argument node kind: '{}', text: '{}'", current.kind(), common::node_text(&current, source).unwrap_or("<error>".to_string()));

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
        }
        "field_access" => {
            let object = common::require_child(&node, "object", "field_access")
                .and_then(|n| parse_expr(&n, source))?;

            let field_node = common::require_child(&node, "field", "field_access")?;
            let field = Ident {
                name: common::node_text(&field_node, source)?,
                span: common::create_span(&field_node),
            };

            ExprKind::Field {
                object: Box::new(object),
                name: field,
            }
        }
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
        }
        "tuple_expr" => {
            let mut elements = Vec::new();
            common::visit_children(&node, |child| {
                if child.kind() == "expression" {
                    elements.push(parse_expr(&child, source)?);
                }
                Ok(())
            })?;
            ExprKind::Tuple(elements)
        }
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
        }
        "hashset_expr" => {
            let mut elements = Vec::new();
            common::visit_children(&node, |child| {
                if child.kind() == "expression" {
                    elements.push(parse_expr(&child, source)?);
                }
                Ok(())
            })?;
            ExprKind::HashSet(elements)
        }
        "let_expr" => {
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

            // Check for type annotation
            let mut type_ann = None;
            if cursor.goto_next_sibling() {
                if cursor.node().kind() == ":" {
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
            let value = Box::new(parse_expr(&cursor.node(), source)?);

            ExprKind::Let {
                pattern,
                type_ann,
                value,
            }
        }
        "struct_expr" => {
            // Parse the path
            let path_node = common::require_child(&node, "path", "struct_expr")?;
            let mut segments = Vec::new();
            common::visit_children(&path_node, |child| {
                if child.kind() == "identifier" {
                    segments.push(Ident {
                        name: common::node_text(&child, source)?,
                        span: common::create_span(&child),
                    });
                }
                Ok(())
            })?;

            // Parse the body
            let body_node = common::require_child(&node, "body", "struct_expr")?;

            let mut fields = Vec::new();
            let mut base = None;

            // Process all children of the body node
            common::visit_children(&body_node, |child| {
                match child.kind() {
                    "field_init" => {
                        // Handle shorthand field
                        if let Some(shorthand) = child.child_by_field_name("shorthand") {
                            let name = Ident {
                                name: common::node_text(&shorthand, source)?,
                                span: common::create_span(&shorthand),
                            };
                            // Create a path expression with the same name
                            let path_expr = Expr::new(
                                ExprKind::Path(vec![name.clone()]),
                                common::create_span(&shorthand),
                            );
                            fields.push((name, path_expr));
                        } else {
                            // Handle normal field
                            let name_node = common::require_child(&child, "name", "field_init")?;
                            let name = Ident {
                                name: common::node_text(&name_node, source)?,
                                span: common::create_span(&name_node),
                            };

                            let value_node = common::require_child(&child, "value", "field_init")?;
                            let value = parse_expr(&value_node, source)?;

                            fields.push((name, value));
                        }
                    }
                    "base_struct" => {
                        let expr_node = common::require_child(&child, "expr", "base_struct")?;
                        base = Some(Box::new(parse_expr(&expr_node, source)?));
                    }
                    _ => {}
                }
                Ok(())
            })?;

            ExprKind::Struct {
                path: segments,
                fields,
                base,
            }
        }
        "paren_expr" => {
            let inner = node
                .child(1)
                .ok_or_else(|| common::node_error(&node, "Parenthesized expression is empty"))?;
            ExprKind::Paren(Box::new(parse_expr(&inner, source)?))
        }
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
                return Err(common::node_error(
                    &node,
                    "If expression has no then branch",
                ));
            }
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(
                    &node,
                    "If expression has no then branch",
                ));
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
        }
        "match_expr" => {
            let mut cursor = node.walk();
            if !cursor.goto_first_child() {
                return Err(common::node_error(
                    &node,
                    "Match expression has no children",
                ));
            }

            // Skip 'match' keyword
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(
                    &node,
                    "Match expression has no scrutinee",
                ));
            }

            // Parse scrutinee
            let scrutinee = Box::new(parse_expr(&cursor.node(), source)?);

            // Skip opening brace
            if !cursor.goto_next_sibling() || cursor.node().kind() != "{" {
                return Err(common::node_error(&node, "Match expression missing '{'"));
            }

            let mut arms = Vec::new();
            // Look for match_arms node
            if let Some(arms_node) = common::find_first_child(&node, "match_arms") {
                // Process each match arm
                common::visit_children(&arms_node, |arm_node| {
                    // Skip comment nodes
                    if arm_node.kind() == "comment" {
                        return Ok(()); // Continue to the next child
                    }

                    if arm_node.kind() == "match_arm" {
                        // Get pattern
                        let pattern_node =
                            common::require_child(&arm_node, "pattern", "match_arm")?;
                        let pattern = pattern::parse_pattern(&pattern_node, source)?;

                        // Get body expression
                        let body_node = common::require_child(&arm_node, "body", "match_arm")?;
                        let expr_result = parse_expr(&body_node, source); // Store the result

                        match expr_result {
                            Ok(expr) => {
                                arms.push((pattern, expr));
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    } else if arm_node.kind() == "," {
                        // Do nothing
                    } else {
                        return Err(common::node_error(
                            &arm_node,
                            &format!("Unexpected node in match arms: {}", arm_node.kind()),
                        ));
                    }
                    Ok(())
                })?;
            }

            ExprKind::Match { scrutinee, arms }
        }
        "lambda_expr" => {
            let mut cursor = node.walk();
            if !cursor.goto_first_child() {
                return Err(common::node_error(
                    &node,
                    "Lambda expression has no children",
                ));
            }

            // Check for generic parameters
            let generic_params = if cursor.node().kind() == "generic_parameters" {
                let params = parse_generic_params(&cursor.node(), source)?;
                cursor.goto_next_sibling();
                Some(params)
            } else {
                None
            };

            // Skip initial '|'
            if cursor.node().kind() != "|" {
                return Err(common::node_error(
                    &node,
                    "Lambda expression missing opening '|'",
                ));
            }
            if !cursor.goto_next_sibling() {
                return Err(common::node_error(
                    &node,
                    "Lambda expression has no parameters",
                ));
            }

            // Parse parameters
            let mut params = Vec::new();
            if cursor.node().kind() == "parameters" {
                let mut param_cursor = cursor.node().walk();
                if param_cursor.goto_first_child() {
                    loop {
                        let current = param_cursor.node();
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
                return Err(common::node_error(
                    &node,
                    "Lambda expression missing closing '|'",
                ));
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
            let body = Box::new(parse_expr(&cursor.node(), source)?);

            ExprKind::Lambda {
                generic_params,
                params,
                body,
            }
        }
        "block" => {
            let mut stmts = Vec::new();
            let mut cursor = node.walk();
            if cursor.goto_first_child() {
                // Skip opening brace
                if cursor.goto_next_sibling() {
                    loop {
                        let current = cursor.node();
                        match current.kind() {
                            "expression" => {
                                stmts.push(parse_expr(&current, source)?);
                            }
                            "block_item" => {
                                if let Some(expr_node) = current.child(0) {
                                    stmts.push(parse_expr(&expr_node, source)?);
                                }
                            }
                            "}" => break,
                            _ => (), // Do nothing
                        }
                        if !cursor.goto_next_sibling() {
                            break; // No more siblings
                        }
                    }
                }
            }
            ExprKind::Block(stmts)
        }
        _ => {
            return Err(common::node_error(
                &node,
                &format!("Unknown expression kind: {}", node.kind()),
            ))
        }
    };

    Ok(Expr::new(kind, span))
}

fn parse_argument(node: &Node, source: &str) -> Result<Argument, SyntaxError> {
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
            name: Some(Ident {
                name: common::node_text(&name, source)?,
                span: common::create_span(&name),
            }),
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

fn parse_generic_params(node: &Node, source: &str) -> Result<Vec<GenericParam>, SyntaxError> {
    let mut params = Vec::new();
    common::visit_children(node, |param| {
        if param.kind() == "generic_param" {
            let is_phantom = param.child(0).map_or(false, |n| n.kind() == "phantom");

            // Find the identifier node within the generic_param node
            let mut identifier_found = false;
            let mut name = Ident {
                name: "".to_string(),
                span: common::create_span(&node),
            }; // Use SourceSpan::from with byte range
            let mut kind = None;

            common::visit_children(&param, |child| {
                if child.kind() == "identifier" {
                    name = Ident {
                        name: common::node_text(&child, source)?,
                        span: common::create_span(&child),
                    };
                    identifier_found = true;
                } else if child.kind() == "kind" {
                    kind = Some(parse_kind(&child, source)?);
                }
                Ok(())
            })?;

            if !identifier_found {
                return Err(common::node_error(
                    &param,
                    "generic_param missing identifier",
                ));
            }

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

fn parse_kind(node: &Node, source: &str) -> Result<Kind, SyntaxError> {
    match node.kind() {
        "star" => Ok(Kind::Star),
        "function_kind" => {
            let param = common::require_child(node, "param", "function_kind")
                .and_then(|n| parse_kind(&n, source))?;
            let ret = common::require_child(node, "return", "function_kind")
                .and_then(|n| parse_kind(&n, source))?;
            Ok(Kind::Function(Box::new(param), Box::new(ret)))
        }
        "tuple_kind" => {
            let mut kinds = Vec::new();
            common::visit_children(node, |child| {
                if child.kind() == "kind" {
                    kinds.push(parse_kind(&child, source)?);
                }
                Ok(())
            })?;
            Ok(Kind::Tuple(kinds))
        }
        _ => Err(common::node_error(
            node,
            &format!("Unknown kind: {}", node.kind()),
        )),
    }
}

fn parse_parameter(node: &Node, source: &str) -> Result<Parameter, SyntaxError> {
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

    let is_variadic = node.child(0).map_or(false, |n| n.kind() == "...");

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
    use crate::{
        ast::{common::Literal, PatternKind},
        parser::print_tree_structure,
    };

    // *** UPDATED HELPER ***
    // Helper function to find the main expression node within a test function wrapper
    fn find_test_expression_node<'a>(tree: &'a Tree, source: &'a str) -> Option<Node<'a>> {
        let root_node = tree.root_node();
        // Expected structure: source_file -> item -> function_item -> body (expression)
        root_node.child(0).and_then(|item_node| {
            // Skip visibility if present
            let actual_item_node = if item_node.child(0).map_or(false, |c| c.kind() == "visibility") {
                item_node.child(1)
            } else {
                item_node.child(0)
            }?; // Use ? for Option early return

            if actual_item_node.kind() == "function_item" {
                 // The body is a direct child field named "body"
                actual_item_node.child_by_field_name("body").and_then(|body_node| {
                    // The body node itself might be the expression (e.g., binary_expr)
                    // or it might be an "expression" node wrapping the actual expression.
                    if body_node.kind() == "expression" {
                        // If it's an expression wrapper, get its first *named* child
                         body_node.named_child(0).or_else(|| { // Fallback if no named child
                             println!("Warning: Expression wrapper node kind '{}' has no named child. Source: {}", body_node.kind(), source);
                             body_node.child(0) // Try first unnamed child as last resort
                         })
                    } else {
                        // If not an "expression" wrapper, assume the body node IS the expression
                        Some(body_node)
                    }
                })
            } else {
                println!("Warning: Expected function_item, found {}. Source: {}", actual_item_node.kind(), source);
                None
            }
        })
    }

    // Helper to parse expression wrapped in a function for testing standard cases
    fn test_expr_node(source: &str) -> Result<Expr, SyntaxError> {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;
        let tree = parser.parse(source, None).unwrap();
        println!("--- Tree Structure for Test Function ---");
        println!("Source: {}", source);
        crate::parser::print_tree_structure(&tree.root_node(), source, 10);
        println!("----------------------------------------");

        // Use the *updated* helper to find the relevant expression node
        let expr_node =
            find_test_expression_node(&tree, source).ok_or_else(|| {
                // Provide more context on failure
                SyntaxError::ParseError {
                    message: format!(
                        "find_test_expression_node failed to find expression for source: {}",
                        source
                    ),
                    span: None,
                }
            })?;

        println!(
            "Parsing expression node kind: '{}' found by test_expr_node helper",
            expr_node.kind()
        );
        parse_expr(&expr_node, source)
    }

    // Helper for testing raw expressions without the function wrapper
    fn test_raw_expr(source: &str) -> Result<Expr, SyntaxError> {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;
        let tree = parser.parse(source, None)
            .ok_or_else(|| SyntaxError::ParseError {
                message: format!("Tree-sitter failed to parse raw source: {}", source),
                span: None,
            })?;
        println!("--- Tree Structure for Raw Expression ---");
        println!("Source: {}", source);
        crate::parser::print_tree_structure(&tree.root_node(), source, 10);
        println!("-----------------------------------------");
        let root_node = tree.root_node();
        if let Some(child_node) = root_node.child(0) {
             let expr_node = if child_node.kind() == "expression_statement" {
                 child_node.named_child(0).unwrap_or(child_node)
             } else {
                 child_node
             };
             println!("Parsing node kind: '{}' for raw expression", expr_node.kind());
             parse_expr(&expr_node, source)
        } else {
             Err(SyntaxError::ParseError {
                 message: format!("Could not find expression node under root for raw source: {}", source),
                 span: Some(SourceSpan::new(root_node.start_byte().into(), root_node.byte_range().len()))
             })
        }
    }

    #[test]
    fn test_literal_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

        // Test integer literal
        let source = "fn test() = 42;";
        println!("\nTesting integer literal:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Int { value: 42, .. })));

        // Test float literal
        let source = "fn test() = 3.14;";
        println!("\nTesting float literal:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Float { value: 3.14, .. })));

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
    fn test_binary_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

        // Test addition
        let source = "fn test() = 1 + 2;";
        println!("\nTesting binary addition:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Add,
                ..
            }
        ));

        // Test subtraction
        let source = "fn test() = 3 - 4;";
        println!("\nTesting binary subtraction:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Sub,
                ..
            }
        ));

        // Test multiplication
        let source = "fn test() = 5 * 6;";
        println!("\nTesting binary multiplication:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Mul,
                ..
            }
        ));

        // Test division
        let source = "fn test() = 8 / 2;";
        println!("\nTesting binary division:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Div,
                ..
            }
        ));

        // Test equality comparison
        let source = "fn test() = x == y;";
        println!("\nTesting binary equality:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Eq,
                ..
            }
        ));

        // Test less than comparison
        let source = "fn test() = a < b;";
        println!("\nTesting binary less than:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Lt,
                ..
            }
        ));

        // Test greater than comparison
        let source = "fn test() = c > d;";
        println!("\nTesting binary greater than:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Gt,
                ..
            }
        ));

        // Test logical AND
        let source = "fn test() = true && false;";
        println!("\nTesting binary logical AND:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::And,
                ..
            }
        ));

        // Test logical OR
        let source = "fn test() = x || y;";
        println!("\nTesting binary logical OR:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Or,
                ..
            }
        ));

        // Test arrow operator
        let source = "fn test() = a -> b;";
        println!("\nTesting binary arrow operator:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Binary {
                op: BinaryOp::Arrow,
                ..
            }
        ));

        Ok(())
    }

    #[test]
    fn test_call_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_unary_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

        // Test negation
        let source = "fn test() = -42;";
        println!("\nTesting unary negation:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Unary {
                op: UnaryOp::Neg,
                ..
            }
        ));

        // Test logical not
        let source = "fn test() = !true;";
        println!("\nTesting unary logical not:");
        println!("Source: {}", source);
        let expr = test_expr_node(source)?;
        assert!(matches!(
            expr.kind,
            ExprKind::Unary {
                op: UnaryOp::Not,
                ..
            }
        ));

        Ok(())
    }

    #[test]
    fn test_field_access_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_array_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_tuple_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_let_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_struct_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_block_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_if_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_match_expressions() -> Result<(), SyntaxError> {
        // Set up the parser
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_parallax::LANGUAGE.into())
            .map_err(|e| SyntaxError::ParserInitError(e.to_string()))?;

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
    fn test_lambda_expressions() -> Result<(), SyntaxError> {
        let source = "fn test() = |x| => x + 1;";
        println!("\nTesting lambda expression (in func): {}", source);
        let expr = test_expr_node(source)?;
        // Ensure the lambda span covers the entire |x| => x + 1 part
        let lambda_span = SourceSpan::new(12.into(), 12); // Start at 12, length 12 -> [12..24]
        let param_span = SourceSpan::new(13.into(), 1);
        let param_ident_span = SourceSpan::new(13.into(), 1);
        let body_span = SourceSpan::new(19.into(), 5);
        let body_left_span = SourceSpan::new(19.into(), 1);
        let body_right_span = SourceSpan::new(23.into(), 1);
        let expected_params = vec![Parameter {
            pattern: crate::ast::pattern::Pattern {
                kind: PatternKind::Identifier(Ident { name: "x".to_string(), span: param_ident_span }),
                span: param_span,
            },
            ty: None, default_value: None, is_variadic: false, span: param_span,
        }];
        let expected_body = Box::new(Expr::new(
            ExprKind::Binary {
                left: Box::new(Expr::new(ExprKind::Path(vec![Ident { name: "x".to_string(), span: body_left_span }]), body_left_span)),
                op: BinaryOp::Add,
                right: Box::new(Expr::new(ExprKind::Literal(Literal::Int { value: 1, suffix: None }), body_right_span)),
            },
            body_span,
        ));

        // Add debug prints here
        println!("Debug span check (test_lambda_expressions):");
        println!("  expr.span: {:?}", expr.span);
        println!("  lambda_span: {:?}", lambda_span);
        assert_eq!(expr.span, lambda_span, "Lambda expression span mismatch");

        // Match on a reference and borrow fields using ref
        match &expr.kind { // Match on &expr.kind
            ExprKind::Lambda { generic_params, params, body } => { // body is now &&Box<Expr>
                assert_eq!(*generic_params, None, "Lambda generic params mismatch"); // Deref Option
                assert_eq!(params, &expected_params, "Lambda params mismatch"); // Compare Vecs by ref
                assert_eq!(body, &expected_body, "Lambda body mismatch"); // Compare &Box<Expr> with &Box<Expr>
            },
             _ => panic!("Expected Lambda expression kind, got {:?}", expr.kind),
        }

        println!("Actual parsed expr.kind: {:?}", expr.kind); // Can now borrow expr.kind again

        Ok(())
    }

    #[test]
    fn test_raw_lambda() -> Result<(), SyntaxError> {
        let source = "fn test() = |x| => x + 1;"; 
        println!("\nTesting lambda expression (in func via test_raw_lambda): {}", source);
        let expr = test_expr_node(source)?;
        // Ensure the lambda span covers the entire |x| => x + 1 part
        let lambda_span = SourceSpan::new(12.into(), 12); // Start at 12, length 12 -> [12..24]
        let param_span = SourceSpan::new(13.into(), 1);
        let param_ident_span = SourceSpan::new(13.into(), 1);
        let body_span = SourceSpan::new(19.into(), 5);
        let body_left_span = SourceSpan::new(19.into(), 1);
        let body_right_span = SourceSpan::new(23.into(), 1);
        let expected_params = vec![Parameter {
             pattern: crate::ast::pattern::Pattern {
                 kind: PatternKind::Identifier(Ident { name: "x".to_string(), span: param_ident_span }),
                 span: param_span,
             },
            ty: None, default_value: None, is_variadic: false, span: param_span,
        }];
        let expected_body = Box::new(Expr::new(
            ExprKind::Binary {
                left: Box::new(Expr::new(ExprKind::Path(vec![Ident { name: "x".to_string(), span: body_left_span }]), body_left_span)),
                op: BinaryOp::Add,
                right: Box::new(Expr::new(ExprKind::Literal(Literal::Int { value: 1, suffix: None }), body_right_span)),
            },
            body_span,
        ));

        // Add debug prints here
        println!("Debug span check (test_raw_lambda):");
        println!("  expr.span: {:?}", expr.span);
        println!("  lambda_span: {:?}", lambda_span);
        assert_eq!(expr.span, lambda_span, "Lambda expression span mismatch (test_raw_lambda)");

        // Match on a reference and borrow fields using ref
         match &expr.kind { // Match on &expr.kind
            ExprKind::Lambda { generic_params, params, body } => {
                assert_eq!(*generic_params, None, "Lambda generic params mismatch (raw)"); // Deref Option
                assert_eq!(params, &expected_params, "Lambda params mismatch (raw)"); // Compare Vecs by ref
                assert_eq!(body, &expected_body, "Lambda body mismatch (raw)"); // Compare &Box<Expr> with &Box<Expr>
            },
             _ => panic!("Expected Lambda expression kind, got {:?} (raw)", expr.kind),
        }

        println!("Actual parsed lambda expr.kind (test_raw_lambda): {:?}", expr.kind); // Can now borrow expr.kind again

        Ok(())
    }

    // ... (Keep other existing tests) ...

} // end mod tests
