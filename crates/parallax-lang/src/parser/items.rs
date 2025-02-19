use tree_sitter::Node;
use crate::error::ParallaxError;
use crate::ast::common::{Span, Ident};
use crate::ast::items::*;
use crate::ast::expr::{GenericParam, Expr, ExprKind};
use crate::ast::types::Type;
use crate::ast::pattern::{Pattern, PatternKind};
use super::types::parse_type;
use super::expr;
use super::pattern;
use super::common::{self, create_span, node_text};

pub fn parse_item(node: &Node, source: &str) -> Result<Item, ParallaxError> {
    let span = create_span(node);

    match node.kind() {
        "item" => {
            // Get the actual item node (after optional visibility)
            let visibility = parse_visibility(node);
            let item_node = if visibility {
                node.child(1)
            } else {
                node.child(0)
            }.ok_or_else(|| common::node_error(node, "Item node has no content"))?;

            let kind = match item_node.kind() {
                "type_def" => ItemKind::TypeDef(parse_type_def(&item_node, source)?),
                "function" => ItemKind::Function(parse_function(&item_node, source)?),
                "enum" => ItemKind::Enum(parse_enum(&item_node, source)?),
                "struct" => ItemKind::Struct(parse_struct(&item_node, source)?),
                "trait" => ItemKind::Trait(parse_trait(&item_node, source)?),
                "impl" => ItemKind::Impl(parse_impl(&item_node, source)?),
                "module" => ItemKind::Module(parse_module(&item_node, source)?),
                "use" => ItemKind::Use(parse_use_decl(&item_node, source)?),
                _ => return Err(common::node_error(&item_node, &format!("Unsupported item type: {}", item_node.kind()))),
            };

            Ok(Item { kind, visibility, span })
        }
        _ => Err(common::node_error(node, &format!("Expected item node, got {}", node.kind()))),
    }
}

fn parse_type_def(node: &Node, source: &str) -> Result<TypeDef, ParallaxError> {
    println!("\nParsing type definition node: {}", node.to_sexp());
    let span = common::create_span(node);

    // Get the name
    let name = common::require_child(node, "name", "type_def")
        .and_then(|n| common::node_text(&n, source))
        .map(Ident)?;
    println!("Found name: {}", name.0);

    let generic_params = parse_generic_params(node, source)?;
    let where_clause = parse_where_clause(node, source)?;

    // Get the type value
    let type_node = common::require_child(node, "value", "type_def")?;
    let ty = parse_type(&type_node, source)?;

    Ok(TypeDef {
        name,
        generic_params,
        ty,
        where_clause,
        span,
    })
}

// Helper function to parse generic parameters
fn parse_generic_params(node: &Node, source: &str) -> Result<Option<Vec<GenericParam>>, ParallaxError> {
    println!("\n=== Parsing Generic Parameters ===");
    println!("Input node: kind='{}', text='{}'", node.kind(), node.utf8_text(source.as_bytes()).unwrap_or(""));
    
    let generic_params_node = common::find_first_child(node, "generic_parameters");
    println!("Looking for generic_parameters node...");
    
    if generic_params_node.is_none() {
        println!("No generic_parameters node found");
        return Ok(None);
    }

    let mut params = Vec::new();
    let params_node = generic_params_node.unwrap();
    println!("Found generic_parameters node: text='{}'", params_node.utf8_text(source.as_bytes()).unwrap_or(""));
    
    common::visit_children(&params_node, |current| {
        println!("Processing child: kind='{}', text='{}'", 
            current.kind(),
            current.utf8_text(source.as_bytes()).unwrap_or("")
        );
        
        if current.kind() == "generic_param" {
            let is_phantom = current.child(0)
                .map_or(false, |n| n.kind() == "phantom");
            
            // Find the identifier node within the generic_param node
            let mut cursor = current.walk();
            if cursor.goto_first_child() {
                loop {
                    let child = cursor.node();
                    println!("  Looking at: kind='{}', text='{}'",
                        child.kind(),
                        child.utf8_text(source.as_bytes()).unwrap_or("")
                    );
                    
                    if child.kind() == "identifier" {
                        let name = common::node_text(&child, source)
                            .map(Ident)?;
                        println!("  Found identifier: '{}' (phantom: {})", name.0, is_phantom);
                        params.push(GenericParam {
                            is_phantom,
                            name,
                            kind: None,
                        });
                        break;
                    }
                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
            }
        }
        Ok(())
    })?;

    println!("Finished parsing generic parameters. Found {} params", params.len());
    if params.is_empty() {
        println!("No parameters found, returning None");
        Ok(None)
    } else {
        println!("Returning {} parameters", params.len());
        Ok(Some(params))
    }
}

// Helper function to parse where clauses
fn parse_where_clause(node: &Node, source: &str) -> Result<Option<WhereClause>, ParallaxError> {
    println!("\n=== Parsing Where Clause ===");
    println!("Input node: kind='{}', text='{}'", node.kind(), node.utf8_text(source.as_bytes()).unwrap_or(""));
    
    let where_node = common::find_first_child(node, "where_clause");
    println!("Looking for where_clause node...");
    
    if where_node.is_none() {
        println!("No where_clause node found");
        return Ok(None);
    }

    let where_node = where_node.unwrap();
    println!("Found where_clause node: text='{}', s-expr: {}", 
        where_node.utf8_text(source.as_bytes()).unwrap_or(""),
        where_node.to_sexp()
    );
    let span = common::create_span(&where_node);

    let mut predicates = Vec::new();
    println!("\nProcessing where clause predicates:");
    common::visit_children(&where_node, |current| {
        println!("  Visiting node: kind='{}', text='{}', s-expr: {}", 
            current.kind(),
            current.utf8_text(source.as_bytes()).unwrap_or(""),
            current.to_sexp()
        );
        
        if current.kind() == "where_pred" {
            println!("  Found where predicate");
            let ty = common::require_child(current, "type", "where_pred")
                .and_then(|n| parse_type(&n, source))?;
            println!("  Parsed type");

            let bounds_node = common::require_child(current, "bounds", "where_pred")?;
            println!("  Found bounds node: s-expr: {}", bounds_node.to_sexp());

            // Print all direct children of bounds node
            println!("  Direct children of bounds node:");
            let mut cursor = bounds_node.walk();
            if cursor.goto_first_child() {
                loop {
                    let child = cursor.node();
                    println!("    - kind='{}', text='{}', s-expr: {}", 
                        child.kind(),
                        child.utf8_text(source.as_bytes()).unwrap_or(""),
                        child.to_sexp()
                    );
                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
            }

            let mut bounds = Vec::new();
            // If the bounds node itself is a path, parse it directly
            if bounds_node.kind() == "path" {
                bounds.push(parse_type(&bounds_node, source)?);
                println!("    Added bound from bounds node (path)");
            } else {
                // Otherwise visit children looking for paths
                common::visit_children(&bounds_node, |bound| {
                    println!("    Processing bound: kind='{}', text='{}', s-expr: {}", 
                        bound.kind(),
                        bound.utf8_text(source.as_bytes()).unwrap_or(""),
                        bound.to_sexp()
                    );
                    if bound.kind() == "path" {
                        bounds.push(parse_type(bound, source)?);
                        println!("    Added bound from child path");
                    }
                    Ok(())
                })?;
            }

            println!("  Adding predicate with {} bounds", bounds.len());
            predicates.push(WherePredicate {
                ty,
                bounds,
                span: common::create_span(current),
            });
        }
        Ok(())
    })?;

    println!("Finished parsing where clause. Found {} predicates", predicates.len());
    Ok(Some(WhereClause { predicates, span }))
}

// Helper function to parse visibility
fn parse_visibility(node: &Node) -> bool {
    node.child(0).map_or(false, |n| n.kind() == "visibility")
}

fn parse_function(node: &Node, source: &str) -> Result<Function, ParallaxError> {
    let span = common::create_span(node);

    // Parse function signature
    let sig_node = common::require_child(node, "sig", "function")?;

    // Get name
    let name = common::require_child(&sig_node, "name", "function")
        .and_then(|n| common::node_text(&n, source))
        .map(Ident)?;

    // Parse generic parameters
    let generic_params = parse_generic_params(&sig_node, source)?;

    // Parse parameters
    let params = parse_parameters(&sig_node, source)?;

    // Parse return type
    let return_type = if let Some(return_type_node) = common::get_child(&sig_node, "return_type") {
        Some(parse_type(&return_type_node, source)?)
    } else {
        None
    };

    // Parse where clause
    let where_clause = parse_where_clause(&sig_node, source)?;

    // Parse body
    let body_node = common::require_child(node, "body", "function")?;
    let body = Box::new(expr::parse_expr(&body_node, source)?);

    Ok(Function {
        name,
        generic_params,
        params,
        return_type,
        where_clause,
        body,
        span,
    })
}

fn parse_parameters(node: &Node, source: &str) -> Result<Vec<Parameter>, ParallaxError> {
    let params_node = common::get_child(node, "params");
    if params_node.is_none() {
        return Ok(Vec::new());
    }

    let mut params = Vec::new();
    common::visit_children(&params_node.unwrap(), |current| {
        if current.kind() == "parameter" {
            let span = common::create_span(current);

            // Check if this is a variadic parameter
            let is_variadic = current.child(0)
                .map_or(false, |n| n.kind() == "...");

            // Check if this is a self parameter
            let is_self = current.child(0)
                .map_or(false, |n| n.kind() == "self");

            // Parse pattern
            let pattern = if is_self {
                // For self parameters, create an identifier pattern with "self"
                Pattern {
                    kind: PatternKind::Identifier(Ident("self".to_string())),
                    span,
                }
            } else if is_variadic {
                common::require_child(current, "name", "parameter")
                    .and_then(|n| pattern::parse_pattern(&n, source))?
            } else {
                common::require_child(current, "pattern", "parameter")
                    .and_then(|n| pattern::parse_pattern(&n, source))?
            };

            // Parse type annotation
            let ty = common::get_child(current, "type")
                .map(|n| parse_type(&n, source))
                .transpose()?;

            // Parse default value
            let default_value = common::get_child(current, "value")
                .map(|n| expr::parse_expr(&n, source))
                .transpose()?;

            params.push(Parameter {
                pattern,
                ty,
                default_value,
                is_variadic,
                span,
            });
        }
        Ok(())
    })?;

    Ok(params)
}

fn parse_enum(node: &Node, source: &str) -> Result<EnumDef, ParallaxError> {
    println!("\n=== Parsing Enum ===");
    println!("Input node: kind='{}', text='{}'", node.kind(), node.utf8_text(source.as_bytes()).unwrap_or(""));
    let span = common::create_span(node);

    // Get name
    let name = common::require_child(node, "name", "enum")
        .and_then(|n| common::node_text(&n, source))
        .map(Ident)?;
    println!("Found enum name: {}", name.0);

    // Parse generic parameters
    let generic_params = parse_generic_params(node, source)?;

    // Parse where clause
    let where_clause = parse_where_clause(node, source)?;

    // Parse variants
    let mut variants = Vec::new();
    println!("\nLooking for enum_variants node...");
    if let Some(variants_node) = common::find_first_child(node, "enum_variants") {
        println!("Found enum_variants node: {}", variants_node.to_sexp());
        let mut cursor = variants_node.walk();
        if cursor.goto_first_child() {
            loop {
                let current = cursor.node();
                println!("Visiting child node: kind='{}', text='{}'", 
                    current.kind(), 
                    current.utf8_text(source.as_bytes()).unwrap_or("")
                );
                if current.kind() == "enum_variant" {
                    println!("Found enum_variant, parsing...");
                    variants.push(parse_enum_variant(&current, source)?);
                    println!("Successfully parsed variant");
                }
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
    } else {
        println!("No enum_variants node found!");
    }
    println!("Found {} variants", variants.len());

    Ok(EnumDef {
        name,
        generic_params,
        where_clause,
        variants,
        span,
    })
}

fn parse_enum_variant(node: &Node, source: &str) -> Result<EnumVariant, ParallaxError> {
    println!("\n=== Parsing Enum Variant ===");
    println!("Input node: kind='{}', text='{}'", node.kind(), node.utf8_text(source.as_bytes()).unwrap_or(""));
    let span = common::create_span(node);

    // Get name using field name
    println!("Looking for identifier node...");
    let name = common::find_first_child(node, "identifier")
        .ok_or_else(|| ParallaxError::NodeError {
            message: "enum_variant missing identifier".to_string(),
            span: Some(span),
            node_type: node.kind().to_string(),
        })
        .and_then(|n| common::node_text(&n, source))
        .map(Ident)?;
    println!("Found variant name: {}", name.0);

    // Parse variant kind
    let kind = if let Some(struct_body) = common::get_child(node, "struct_body") {
        println!("Found struct body");
        let mut fields = Vec::new();
        common::visit_children(&struct_body, |current| {
            if current.kind() == "struct_field" {
                fields.push(parse_struct_field(current, source)?);
            }
            Ok(())
        })?;
        
        EnumVariantKind::Struct(fields)
    } else if let Some(tuple_body) = common::get_child(node, "tuple_body") {
        println!("Found tuple body");
        let mut types = Vec::new();
        common::visit_children(&tuple_body, |current| {
            if current.kind() == "type" {
                types.push(parse_type(current, source)?);
            }
            Ok(())
        })?;
        
        EnumVariantKind::Tuple(types)
    } else {
        println!("No body found, treating as unit variant");
        EnumVariantKind::Unit
    };

    Ok(EnumVariant { name, kind, span })
}

fn parse_struct_field(node: &Node, source: &str) -> Result<StructField, ParallaxError> {
    let span = create_span(node);
    let visibility = node.child_by_field_name("visibility").is_some();
    
    // Find the identifier node directly
    let name = common::find_first_child(node, "identifier")
        .ok_or_else(|| ParallaxError::NodeError {
            message: "struct_field missing identifier".to_string(),
            span: Some(span),
            node_type: node.kind().to_string(),
        })
        .and_then(|n| common::node_text(&n, source))
        .map(Ident)?;
    
    // Find the type node directly
    let type_node = common::find_first_child(node, "type")
        .ok_or_else(|| ParallaxError::NodeError {
            message: "struct_field missing type".to_string(),
            span: Some(span),
            node_type: node.kind().to_string(),
        })?;
    let ty = parse_type(&type_node, source)?;
    
    Ok(StructField {
        name,
        ty,
        visibility,
        span,
    })
}

fn parse_struct(node: &Node, source: &str) -> Result<StructDef, ParallaxError> {
    let span = create_span(node);
    let name = common::require_child(node, "name", "struct")
        .and_then(|n| common::node_text(&n, source))
        .map(Ident)?;

    let generic_params = parse_generic_params(node, source)?;
    let where_clause = parse_where_clause(node, source)?;
    let mut fields = Vec::new();

    // Handle struct body
    if let Some(body_node) = common::get_child(node, "body") {
        if body_node.kind() == "struct_body" {
            common::visit_children(&body_node, |current| {
                if current.kind() == "struct_field" {
                    fields.push(parse_struct_field(current, source)?);
                }
                Ok(())
            })?;
        }
    }
    // Handle tuple struct body
    else if let Some(tuple_body) = common::get_child(node, "tuple_body") {
        let mut index = 0;
        common::visit_children(&tuple_body, |current| {
            if current.kind() == "type" {
                fields.push(StructField {
                    name: Ident(format!("{}", index)),
                    ty: parse_type(current, source)?,
                    visibility: true,  // Tuple struct fields are always public
                    span: common::create_span(current),
                });
                index += 1;
            }
            Ok(())
        })?;
    }

    Ok(StructDef {
        name,
        generic_params,
        where_clause,
        fields,
        span,
    })
}

fn parse_trait(node: &Node, source: &str) -> Result<TraitDef, ParallaxError> {
    let span = common::create_span(node);

    // Get name
    let name = common::require_child(node, "name", "trait")
        .and_then(|n| common::node_text(&n, source))
        .map(Ident)?;

    // Parse generic parameters
    let generic_params = parse_generic_params(node, source)?;

    // Parse where clause
    let where_clause = parse_where_clause(node, source)?;

    // Parse supertraits
    let mut supertraits = Vec::new();
    if let Some(bounds_node) = common::get_child(node, "bounds") {
        common::visit_children(&bounds_node, |current| {
            if current.kind() == "path" {
                supertraits.push(parse_type(current, source)?);
            }
            Ok(())
        })?;
    }

    // Parse trait items
    let mut items = Vec::new();
    if let Some(items_node) = common::get_child(node, "items") {
        common::visit_children(&items_node, |current| {
            if current.kind() == "trait_item" {
                items.push(parse_trait_item(current, source)?);
            }
            Ok(())
        })?;
    }

    Ok(TraitDef {
        name,
        generic_params,
        where_clause,
        supertraits,
        items,
        span,
    })
}

fn parse_trait_item(node: &Node, source: &str) -> Result<TraitItem, ParallaxError> {
    println!("Parsing trait item node: {}", node.to_sexp());
    println!("Node kind: {}", node.kind());
    println!("Node text: {}", node.utf8_text(source.as_bytes()).unwrap_or(""));
    
    let span = common::create_span(node);
    
    // Get the function signature node (first child)
    let sig_node = node.child(0).ok_or_else(|| ParallaxError::NodeError {
        message: "trait_item missing function signature".to_string(),
        span: Some(span),
        node_type: node.kind().to_string(),
    })?;
    println!("Found signature node: {}", sig_node.to_sexp());
    
    // Get the function name
    let name_node = common::require_child(&sig_node, "name", "function")?;
    let name = common::node_text(&name_node, source).map(Ident)?;
    println!("Found function name: {}", name.0);
    
    // Parse generic parameters
    let generic_params = parse_generic_params(&sig_node, source)?;
    println!("Generic params parsed: {}", generic_params.is_some());

    // Parse parameters
    let params = parse_parameters(&sig_node, source)?;
    println!("Parameters parsed: {} params found", params.len());

    // Parse return type if present
    let return_type = if let Some(return_node) = common::get_child(&sig_node, "return_type") {
        println!("Found return type node");
        Some(parse_type(&return_node, source)?)
    } else {
        println!("No return type found");
        None
    };

    // Parse where clause
    let where_clause = parse_where_clause(&sig_node, source)?;
    println!("Where clause parsed: {}", where_clause.is_some());

    // Parse default implementation if present
    let default_impl = if let Some(default_node) = common::get_child(node, "default_impl") {
        println!("Found default implementation node");
        Some(expr::parse_expr(&default_node, source)?)
    } else {
        println!("No default implementation found");
        None
    };

    // Create the function with either the default implementation or a placeholder
    let body = if let Some(expr) = default_impl.as_ref() {
        println!("Using default implementation as body");
        expr.clone()
    } else {
        println!("Using signature node as placeholder for required method");
        expr::parse_expr(&sig_node, source)?
    };

    let function = Function {
        name,
        generic_params,
        params,
        return_type,
        where_clause,
        body: Box::new(body),
        span,
    };

    println!("Successfully created function for trait item");

    Ok(TraitItem {
        function,
        default_impl,
        span,
    })
}

fn parse_impl(node: &Node, source: &str) -> Result<ImplDef, ParallaxError> {
    let span = common::create_span(node);

    // Parse generic parameters
    let generic_params = parse_generic_params(node, source)?;

    // Parse trait type if this is a trait implementation
    let trait_type = common::get_child(node, "trait")
        .map(|n| parse_type(&n, source))
        .transpose()?;

    // Parse self type
    let self_type = common::require_child(node, "type", "impl")
        .and_then(|n| parse_type(&n, source))?;

    // Parse where clause
    let where_clause = parse_where_clause(node, source)?;

    // Parse impl items
    let mut items = Vec::new();
    common::visit_children(node, |current| {
        if current.kind() == "function" {
            items.push(parse_function(current, source)?);
        }
        Ok(())
    })?;

    Ok(ImplDef {
        generic_params,
        trait_type,
        self_type,
        where_clause,
        items,
        span,
    })
}

/// Parse a module declaration
pub fn parse_module(node: &Node, source: &str) -> Result<Module, ParallaxError> {
    println!("\nParsing module node: {}", node.to_sexp());
    println!("Module node kind: {}", node.kind());
    
    // Get the module name by finding the identifier node
    let mut cursor = node.walk();
    let mut name = None;
    
    if cursor.goto_first_child() {
        println!("\nLooking for module name:");
        loop {
            let child = cursor.node();
            println!("Child node: {} [{}]", child.kind(), child.to_sexp());
            
            if child.kind() == "identifier" {
                println!("Found identifier node");
                name = Some(Ident(common::node_text(&child, source)?));
                break;
            }
            
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
    
    let name = name.ok_or_else(|| ParallaxError::NodeError {
        message: "Module missing name".to_string(),
        span: Some(create_span(node)),
        node_type: node.kind().to_string(),
    })?;
    
    println!("Module name: {}", name.0);
    
    // Get the module body
    let mut items = Vec::new();
    let mut cursor = node.walk();
    
    if cursor.goto_first_child() {
        println!("\nTraversing module children:");
        loop {
            let child = cursor.node();
            println!("Child node: {} [{}]", child.kind(), child.to_sexp());
            
            match child.kind() {
                "item" => {
                    println!("Found item node, attempting to parse");
                    match parse_item(&child, source) {
                        Ok(item) => {
                            println!("Successfully parsed item");
                            items.push(item);
                        },
                        Err(e) => println!("Failed to parse item: {}", e),
                    }
                },
                "mod" | "identifier" | "{" | "}" => {
                    println!("Skipping structural node: {}", child.kind());
                    // Skip structural nodes
                },
                _ => println!("Unexpected node kind: {}", child.kind()),
            }
            
            if !cursor.goto_next_sibling() {
                println!("No more siblings");
                break;
            }
        }
    } else {
        println!("Module has no children");
    }
    
    println!("\nFinished parsing module:");
    println!("Name: {}", name.0);
    println!("Items count: {}", items.len());
    
    Ok(Module {
        name,
        items,
        span: create_span(node),
    })
}

fn parse_use_decl(node: &Node, source: &str) -> Result<UseDecl, ParallaxError> {
    let span = common::create_span(node);

    // Parse use tree
    let tree_node = common::require_child(node, "tree", "use")?;
    let tree = parse_use_tree(&tree_node, source)?;

    Ok(UseDecl { tree, span })
}

fn parse_use_tree(node: &Node, source: &str) -> Result<UseTree, ParallaxError> {
    println!("\nParsing use tree node: {} [{}-{}]", node.kind(), node.start_byte(), node.end_byte());
    let span = common::create_span(node);
    
    let kind = match node.kind() {
        "use_tree" => {
            let mut cursor = node.walk();
            if !cursor.goto_first_child() {
                println!("Error: Empty use tree - no children");
                return Err(common::node_error(node, "Empty use tree"));
            }

            println!("First child node: {} [{}-{}]", cursor.node().kind(), cursor.node().start_byte(), cursor.node().end_byte());
            if let Ok(text) = cursor.node().utf8_text(source.as_bytes()) {
                println!("Node text: {}", text);
            }

            match cursor.node().kind() {
                "identifier" | "self" | "super" | "crate" => {
                    println!("Found {} node", cursor.node().kind());
                    let segment = match cursor.node().kind() {
                        "identifier" => Ident(common::node_text(&cursor.node(), source)?),
                        special @ ("self" | "super" | "crate") => Ident(special.to_string()),
                        _ => unreachable!()
                    };
                    println!("Parsed segment: {}", segment.0);
                    
                    // Check for alias
                    let mut alias = None;
                    if cursor.goto_next_sibling() {
                        println!("Next sibling after {}: {} [{}-{}]", 
                            cursor.node().kind(), 
                            cursor.node().kind(), 
                            cursor.node().start_byte(), 
                            cursor.node().end_byte()
                        );
                        if cursor.node().kind() == "as" {
                            println!("Found 'as' keyword");
                            if cursor.goto_next_sibling() {
                                println!("Found node after 'as': {}", cursor.node().kind());
                                if cursor.node().kind() == "identifier" {
                                    alias = Some(Ident(common::node_text(&cursor.node(), source)?));
                                    println!("Parsed alias: {}", alias.as_ref().unwrap().0);
                                }
                            }
                        }
                    }

                    // Reset cursor to identifier/special keyword
                    cursor = node.walk();
                    cursor.goto_first_child();
                    println!("\nReset cursor to: {} [{}-{}]", 
                        cursor.node().kind(),
                        cursor.node().start_byte(),
                        cursor.node().end_byte()
                    );

                    // Check for sub-tree
                    let mut sub_tree = None;
                    println!("\nBefore first goto_next_sibling for sub-tree check:");
                    println!("Current cursor node: {} [{}-{}]", 
                        cursor.node().kind(),
                        cursor.node().start_byte(),
                        cursor.node().end_byte()
                    );
                    
                    let has_next = cursor.goto_next_sibling();
                    println!("goto_next_sibling returned: {}", has_next);
                    
                    if has_next {
                        println!("\nAfter first goto_next_sibling:");
                        println!("Current cursor node: {} [{}-{}]", 
                            cursor.node().kind(), 
                            cursor.node().start_byte(), 
                            cursor.node().end_byte()
                        );
                        
                        // Save the current cursor position
                        let mut current = cursor.node();
                        println!("Saved current node: {} [{}-{}]", 
                            current.kind(),
                            current.start_byte(),
                            current.end_byte()
                        );
                        
                        if current.kind() == "::" {
                            println!("\nFound '::', attempting to get next sibling");
                            // Try to get the next sibling after ::
                            let has_next_after_colons = cursor.goto_next_sibling();
                            println!("goto_next_sibling after '::' returned: {}", has_next_after_colons);
                            
                            if has_next_after_colons {
                                current = cursor.node();
                                println!("\nAfter second goto_next_sibling:");
                                println!("Current cursor node: {} [{}-{}]", 
                                    current.kind(),
                                    current.start_byte(),
                                    current.end_byte()
                                );
                                
                                if current.kind() == "use_tree" {
                                    println!("\nFound use_tree node, recursing");
                                    println!("use_tree node details: {} [{}-{}]", 
                                        current.kind(),
                                        current.start_byte(),
                                        current.end_byte()
                                    );
                                    sub_tree = Some(Box::new(parse_use_tree(&current, source)?));
                                } else {
                                    println!("ERROR: Expected use_tree node but got: {}", current.kind());
                                }
                            } else {
                                println!("ERROR: Failed to move cursor after '::'");
                            }
                        } else {
                            println!("ERROR: Expected '::' but got: {}", current.kind());
                        }
                    } else {
                        println!("ERROR: Failed to move cursor for sub-tree check");
                    }

                    println!("\nFinal state before creating UseTreeKind:");
                    println!("segment: {}", segment.0);
                    println!("alias: {:?}", alias.as_ref().map(|a| &a.0));
                    println!("has_subtree: {}", sub_tree.is_some());

                    UseTreeKind::Path {
                        segment,
                        alias,
                        sub_tree,
                    }
                },
                "{" => {
                    println!("Found group node");
                    let mut trees = Vec::new();
                    while cursor.goto_next_sibling() {
                        println!("Group child node: {}", cursor.node().kind());
                        if cursor.node().kind() == "use_tree" {
                            trees.push(parse_use_tree(&cursor.node(), source)?);
                        }
                    }
                    println!("Created group with {} trees", trees.len());
                    UseTreeKind::Group(trees)
                },
                "*" => {
                    println!("Found glob node");
                    UseTreeKind::Glob
                },
                _ => {
                    println!("Error: Unknown use tree kind: {}", cursor.node().kind());
                    return Err(common::node_error(&cursor.node(), &format!("Unknown use tree kind: {}", cursor.node().kind())));
                }
            }
        },
        _ => {
            println!("Error: Expected use tree, got {}", node.kind());
            return Err(common::node_error(node, &format!("Expected use tree, got {}", node.kind())));
        }
    };

    Ok(UseTree { kind, span })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::common::{test_utils::*, find_first_child};
    use crate::ast::types::TypeKind;

    // Helper function to parse any item
    fn parse_item_from_source(source: &str) -> Result<Item, ParallaxError> {
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        print_test_tree(&tree.root_node(), source, 10);
        
        if let Some(item) = find_first_child(&tree.root_node(), "item") {
            parse_item(&item, source)
        } else {
            panic!("Could not find item node");
        }
    }

    mod type_def_tests {
        use super::*;

        #[test]
        fn test_simple_type_def() {
            let item = parse_item_from_source("type MyInt = i32;").unwrap();
            match item.kind {
                ItemKind::TypeDef(type_def) => {
                    assert_eq!(type_def.name.0, "MyInt");
                    assert!(type_def.generic_params.is_none());
                    assert!(type_def.where_clause.is_none());
                    match type_def.ty.kind {
                        TypeKind::Path(path) => {
                            assert_eq!(path.len(), 1);
                            assert_eq!(path[0].0, "i32");
                        },
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected type definition"),
            }
        }

        #[test]
        fn test_generic_type_def() {
            let item = parse_item_from_source("type Result<T, E> = std::result::Result<T, E>;").unwrap();
            match item.kind {
                ItemKind::TypeDef(type_def) => {
                    assert_eq!(type_def.name.0, "Result");
                    // Check generic parameters
                    let generic_params = type_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 2);
                    assert_eq!(generic_params[0].name.0, "T");
                    assert_eq!(generic_params[1].name.0, "E");
                    // Check type
                    match type_def.ty.kind {
                        TypeKind::KindApp(base, args) => {
                            // Check base type
                            match base.kind {
                                TypeKind::Path(path) => {
                                    assert_eq!(path.len(), 3);
                                    assert_eq!(path[0].0, "std");
                                    assert_eq!(path[1].0, "result");
                                    assert_eq!(path[2].0, "Result");
                                },
                                _ => panic!("Expected path type for base"),
                            }
                            // Check type arguments
                            assert_eq!(args.len(), 2);
                            match &args[0].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                                _ => panic!("Expected path type for first argument"),
                            }
                            match &args[1].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "E"),
                                _ => panic!("Expected path type for second argument"),
                            }
                        },
                        _ => panic!("Expected kind application type"),
                    }
                },
                _ => panic!("Expected type definition"),
            }
        }

        #[test]
        fn test_type_def_with_where_clause() {
            let item = parse_item_from_source("type Printable<T> = T where T: Display;").unwrap();
            match item.kind {
                ItemKind::TypeDef(type_def) => {
                    assert_eq!(type_def.name.0, "Printable");
                    // Check generic parameters
                    let generic_params = type_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 1);
                    assert_eq!(generic_params[0].name.0, "T");
                    // Check where clause
                    let where_clause = type_def.where_clause.unwrap();
                    assert_eq!(where_clause.predicates.len(), 1);
                    let pred = &where_clause.predicates[0];
                    match &pred.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                    match &pred.bounds[0].kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Display"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected type definition"),
            }
        }

        #[test]
        fn test_pub_type_def() {
            let item = parse_item_from_source("pub type Meters = f64;").unwrap();
            assert!(item.visibility);
            match item.kind {
                ItemKind::TypeDef(type_def) => {
                    assert_eq!(type_def.name.0, "Meters");
                    match type_def.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected type definition"),
            }
        }
    }

    mod struct_tests {
        use super::*;

        #[test]
        fn test_empty_struct() {
            let item = parse_item_from_source("struct Empty;").unwrap();
            match item.kind {
                ItemKind::Struct(struct_def) => {
                    assert_eq!(struct_def.name.0, "Empty");
                    assert!(struct_def.generic_params.is_none());
                    assert!(struct_def.where_clause.is_none());
                    assert!(struct_def.fields.is_empty());
                },
                _ => panic!("Expected struct definition"),
            }
        }

        #[test]
        fn test_tuple_struct() {
            let item = parse_item_from_source("struct Point(f64, f64);").unwrap();
            match item.kind {
                ItemKind::Struct(struct_def) => {
                    assert_eq!(struct_def.name.0, "Point");
                    assert!(struct_def.generic_params.is_none());
                    assert!(struct_def.where_clause.is_none());
                    assert_eq!(struct_def.fields.len(), 2);
                    // Check first field
                    let field = &struct_def.fields[0];
                    match &field.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                        _ => panic!("Expected path type"),
                    }
                    // Check second field
                    let field = &struct_def.fields[1];
                    match &field.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected struct definition"),
            }
        }

        #[test]
        fn test_named_struct() {
            let item = parse_item_from_source(r#"
                struct Person {
                    name: String,
                    age: u32,
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Struct(struct_def) => {
                    assert_eq!(struct_def.name.0, "Person");
                    assert!(struct_def.generic_params.is_none());
                    assert!(struct_def.where_clause.is_none());
                    assert_eq!(struct_def.fields.len(), 2);
                    // Check name field
                    let field = &struct_def.fields[0];
                    assert_eq!(field.name.0, "name");
                    match &field.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                        _ => panic!("Expected path type"),
                    }
                    // Check age field
                    let field = &struct_def.fields[1];
                    assert_eq!(field.name.0, "age");
                    match &field.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "u32"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected struct definition"),
            }
        }

        #[test]
        fn test_generic_struct() {
            let item = parse_item_from_source(r#"
                struct Container<T> {
                    data: Vec<T>,
                    len: usize,
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Struct(struct_def) => {
                    assert_eq!(struct_def.name.0, "Container");
                    // Check generic parameters
                    let generic_params = struct_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 1);
                    assert_eq!(generic_params[0].name.0, "T");
                    assert!(struct_def.where_clause.is_none());
                    assert_eq!(struct_def.fields.len(), 2);
                    // Check data field
                    let field = &struct_def.fields[0];
                    assert_eq!(field.name.0, "data");
                    match &field.ty.kind {
                        TypeKind::KindApp(base, args) => {
                            // Check base type (Vec)
                            match &base.kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "Vec"),
                                _ => panic!("Expected path type for Vec"),
                            }
                            // Check type argument (T)
                            assert_eq!(args.len(), 1);
                            match &args[0].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                                _ => panic!("Expected path type for T"),
                            }
                        },
                        _ => panic!("Expected kind application type"),
                    }
                    // Check len field
                    let field = &struct_def.fields[1];
                    assert_eq!(field.name.0, "len");
                    match &field.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "usize"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected struct definition"),
            }
        }

        #[test]
        fn test_struct_with_where_clause() {
            let item = parse_item_from_source(r#"
                struct Wrapper<T> where T: Display {
                    inner: T,
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Struct(struct_def) => {
                    assert_eq!(struct_def.name.0, "Wrapper");
                    // Check generic parameters
                    let generic_params = struct_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 1);
                    assert_eq!(generic_params[0].name.0, "T");
                    // Check where clause
                    let where_clause = struct_def.where_clause.unwrap();
                    assert_eq!(where_clause.predicates.len(), 1);
                    let pred = &where_clause.predicates[0];
                    match &pred.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                    match &pred.bounds[0].kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Display"),
                        _ => panic!("Expected path type"),
                    }
                    // Check field
                    assert_eq!(struct_def.fields.len(), 1);
                    let field = &struct_def.fields[0];
                    assert_eq!(field.name.0, "inner");
                    match &field.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected struct definition"),
            }
        }

        #[test]
        fn test_pub_struct() {
            let item = parse_item_from_source(r#"
                pub struct Config {
                    pub name: String,
                    internal: u32,
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Struct(struct_def) => {
                    assert!(item.visibility);
                    assert_eq!(struct_def.name.0, "Config");
                    assert_eq!(struct_def.fields.len(), 2);
                    // Check name field
                    let field = &struct_def.fields[0];
                    assert!(field.visibility);
                    assert_eq!(field.name.0, "name");
                    // Check internal field
                    let field = &struct_def.fields[1];
                    assert!(!field.visibility);
                    assert_eq!(field.name.0, "internal");
                },
                _ => panic!("Expected struct definition"),
            }
        }
    }

    mod enum_tests {
        use super::*;

        #[test]
        fn test_unit_enum() {
            let item = parse_item_from_source(r#"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Enum(enum_def) => {
                    assert_eq!(enum_def.name.0, "Color");
                    assert!(enum_def.generic_params.is_none());
                    assert!(enum_def.where_clause.is_none());
                    assert_eq!(enum_def.variants.len(), 3);
                    // Check variants
                    let variants = &enum_def.variants;
                    assert_eq!(variants[0].name.0, "Red");
                    assert!(matches!(variants[0].kind, EnumVariantKind::Unit));
                    assert_eq!(variants[1].name.0, "Green");
                    assert!(matches!(variants[1].kind, EnumVariantKind::Unit));
                    assert_eq!(variants[2].name.0, "Blue");
                    assert!(matches!(variants[2].kind, EnumVariantKind::Unit));
                },
                _ => panic!("Expected enum definition"),
            }
        }

        #[test]
        fn test_tuple_enum() {
            let item = parse_item_from_source(r#"
                enum Option<T> {
                    Some(T),
                    None,
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Enum(enum_def) => {
                    assert_eq!(enum_def.name.0, "Option");
                    // Check generic parameters
                    let generic_params = enum_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 1);
                    assert_eq!(generic_params[0].name.0, "T");
                    assert!(enum_def.where_clause.is_none());
                    assert_eq!(enum_def.variants.len(), 2);
                    // Check Some variant
                    let some = &enum_def.variants[0];
                    assert_eq!(some.name.0, "Some");
                    match &some.kind {
                        EnumVariantKind::Tuple(types) => {
                            assert_eq!(types.len(), 1);
                            match &types[0].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected tuple variant"),
                    }
                    // Check None variant
                    let none = &enum_def.variants[1];
                    assert_eq!(none.name.0, "None");
                    assert!(matches!(none.kind, EnumVariantKind::Unit));
                },
                _ => panic!("Expected enum definition"),
            }
        }

        #[test]
        fn test_struct_enum() {
            let item = parse_item_from_source(r#"
                enum Message {
                    Quit,
                    Move { x: i32, y: i32 },
                    Write(String),
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Enum(enum_def) => {
                    assert_eq!(enum_def.name.0, "Message");
                    assert!(enum_def.generic_params.is_none());
                    assert!(enum_def.where_clause.is_none());
                    assert_eq!(enum_def.variants.len(), 3);
                    // Check Quit variant
                    let quit = &enum_def.variants[0];
                    assert_eq!(quit.name.0, "Quit");
                    assert!(matches!(quit.kind, EnumVariantKind::Unit));
                    // Check Move variant
                    let move_var = &enum_def.variants[1];
                    assert_eq!(move_var.name.0, "Move");
                    match &move_var.kind {
                        EnumVariantKind::Struct(fields) => {
                            assert_eq!(fields.len(), 2);
                            assert_eq!(fields[0].name.0, "x");
                            match &fields[0].ty.kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                                _ => panic!("Expected path type"),
                            }
                            assert_eq!(fields[1].name.0, "y");
                            match &fields[1].ty.kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected struct variant"),
                    }
                    // Check Write variant
                    let write = &enum_def.variants[2];
                    assert_eq!(write.name.0, "Write");
                    match &write.kind {
                        EnumVariantKind::Tuple(types) => {
                            assert_eq!(types.len(), 1);
                            match &types[0].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected tuple variant"),
                    }
                },
                _ => panic!("Expected enum definition"),
            }
        }

        #[test]
        fn test_enum_with_where_clause() {
            let item = parse_item_from_source(r#"
                enum Either<L, R> where L: Display, R: Debug {
                    Left(L),
                    Right(R),
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Enum(enum_def) => {
                    assert_eq!(enum_def.name.0, "Either");
                    // Check generic parameters
                    let generic_params = enum_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 2);
                    assert_eq!(generic_params[0].name.0, "L");
                    assert_eq!(generic_params[1].name.0, "R");
                    // Check where clause
                    let where_clause = enum_def.where_clause.unwrap();
                    assert_eq!(where_clause.predicates.len(), 2);
                    // Check L: Display
                    let pred = &where_clause.predicates[0];
                    match &pred.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "L"),
                        _ => panic!("Expected path type"),
                    }
                    match &pred.bounds[0].kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Display"),
                        _ => panic!("Expected path type"),
                    }
                    // Check R: Debug
                    let pred = &where_clause.predicates[1];
                    match &pred.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "R"),
                        _ => panic!("Expected path type"),
                    }
                    match &pred.bounds[0].kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Debug"),
                        _ => panic!("Expected path type"),
                    }
                    // Check variants
                    assert_eq!(enum_def.variants.len(), 2);
                    // Check Left variant
                    let left = &enum_def.variants[0];
                    assert_eq!(left.name.0, "Left");
                    match &left.kind {
                        EnumVariantKind::Tuple(types) => {
                            assert_eq!(types.len(), 1);
                            match &types[0].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "L"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected tuple variant"),
                    }
                    // Check Right variant
                    let right = &enum_def.variants[1];
                    assert_eq!(right.name.0, "Right");
                    match &right.kind {
                        EnumVariantKind::Tuple(types) => {
                            assert_eq!(types.len(), 1);
                            match &types[0].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "R"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected tuple variant"),
                    }
                },
                _ => panic!("Expected enum definition"),
            }
        }

        #[test]
        fn test_pub_enum() {
            let item = parse_item_from_source(r#"
                pub enum Result<T, E> {
                    Ok(T),
                    Err(E),
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Enum(enum_def) => {
                    assert!(item.visibility);
                    assert_eq!(enum_def.name.0, "Result");
                    // Check generic parameters
                    let generic_params = enum_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 2);
                    assert_eq!(generic_params[0].name.0, "T");
                    assert_eq!(generic_params[1].name.0, "E");
                    // Check variants
                    assert_eq!(enum_def.variants.len(), 2);
                    // Check Ok variant
                    let ok = &enum_def.variants[0];
                    assert_eq!(ok.name.0, "Ok");
                    match &ok.kind {
                        EnumVariantKind::Tuple(types) => {
                            assert_eq!(types.len(), 1);
                            match &types[0].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected tuple variant"),
                    }
                    // Check Err variant
                    let err = &enum_def.variants[1];
                    assert_eq!(err.name.0, "Err");
                    match &err.kind {
                        EnumVariantKind::Tuple(types) => {
                            assert_eq!(types.len(), 1);
                            match &types[0].kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "E"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected tuple variant"),
                    }
                },
                _ => panic!("Expected enum definition"),
            }
        }
    }

    mod use_tests {
        use super::*;

        // Helper function to parse a use declaration
        fn parse_use_decl(source: &str) -> Result<UseDecl, ParallaxError> {
            let mut parser = create_test_parser();
            let tree = parser.parse(source, None).unwrap();
            print_test_tree(&tree.root_node(), source, 10);
            
            if let Some(item) = find_first_child(&tree.root_node(), "item") {
                let item = parse_item(&item, source)?;
                match item.kind {
                    ItemKind::Use(use_decl) => Ok(use_decl),
                    _ => panic!("Expected use declaration"),
                }
            } else {
                panic!("Could not find item node");
            }
        }

        #[test]
        fn test_parse_use_simple() {
            let use_decl = parse_use_decl("use std::io::Result;").unwrap();
            match use_decl.tree.kind {
                UseTreeKind::Path { segment, alias, sub_tree } => {
                    assert_eq!(segment.0, "std");
                    assert!(alias.is_none());
                    assert!(sub_tree.is_some());
                    let sub_tree = sub_tree.unwrap();
                    match sub_tree.kind {
                        UseTreeKind::Path { segment, alias, sub_tree } => {
                            assert_eq!(segment.0, "io");
                            assert!(alias.is_none());
                            assert!(sub_tree.is_some());
                            let sub_tree = sub_tree.unwrap();
                            match sub_tree.kind {
                                UseTreeKind::Path { segment, alias, sub_tree } => {
                                    assert_eq!(segment.0, "Result");
                                    assert!(alias.is_none());
                                    assert!(sub_tree.is_none());
                                },
                                _ => panic!("Expected path"),
                            }
                        },
                        _ => panic!("Expected path"),
                    }
                },
                _ => panic!("Expected path"),
            }
        }

        #[test]
        fn test_parse_use_single() {
            let use_decl = parse_use_decl("use std;").unwrap();
            match use_decl.tree.kind {
                UseTreeKind::Path { segment, alias, sub_tree } => {
                    assert_eq!(segment.0, "std");
                    assert!(alias.is_none());
                    assert!(sub_tree.is_none());
                },
                _ => panic!("Expected path"),
            }
        }

        #[test]
        fn test_parse_use_with_alias() {
            let use_decl = parse_use_decl("use std::io::Result as IoResult;").unwrap();
            match use_decl.tree.kind {
                UseTreeKind::Path { segment, alias, sub_tree } => {
                    assert_eq!(segment.0, "std");
                    assert!(alias.is_none());
                    assert!(sub_tree.is_some());
                    let sub_tree = sub_tree.unwrap();
                    match sub_tree.kind {
                        UseTreeKind::Path { segment, alias, sub_tree } => {
                            assert_eq!(segment.0, "io");
                            assert!(alias.is_none());
                            assert!(sub_tree.is_some());
                            let sub_tree = sub_tree.unwrap();
                            match sub_tree.kind {
                                UseTreeKind::Path { segment, alias, sub_tree } => {
                                    assert_eq!(segment.0, "Result");
                                    assert_eq!(alias.as_ref().unwrap().0, "IoResult");
                                    assert!(sub_tree.is_none());
                                },
                                _ => panic!("Expected path"),
                            }
                        },
                        _ => panic!("Expected path"),
                    }
                },
                _ => panic!("Expected path"),
            }
        }

        #[test]
        fn test_parse_use_glob() {
            let use_decl = parse_use_decl("use std::io::*;").unwrap();
            match use_decl.tree.kind {
                UseTreeKind::Path { segment, alias, sub_tree } => {
                    assert_eq!(segment.0, "std");
                    assert!(alias.is_none());
                    assert!(sub_tree.is_some());
                    let sub_tree = sub_tree.unwrap();
                    match sub_tree.kind {
                        UseTreeKind::Path { segment, alias, sub_tree } => {
                            assert_eq!(segment.0, "io");
                            assert!(alias.is_none());
                            assert!(sub_tree.is_some());
                            let sub_tree = sub_tree.unwrap();
                            match sub_tree.kind {
                                UseTreeKind::Glob => {},
                                _ => panic!("Expected glob"),
                            }
                        },
                        _ => panic!("Expected path"),
                    }
                },
                _ => panic!("Expected path"),
            }
        }

        #[test]
        fn test_parse_use_group() {
            let use_decl = parse_use_decl("use std::{io, fs};").unwrap();
            match use_decl.tree.kind {
                UseTreeKind::Path { segment, alias, sub_tree } => {
                    assert_eq!(segment.0, "std");
                    assert!(alias.is_none());
                    assert!(sub_tree.is_some());
                    let sub_tree = sub_tree.unwrap();
                    match sub_tree.kind {
                        UseTreeKind::Group(trees) => {
                            assert_eq!(trees.len(), 2);
                            match &trees[0].kind {
                                UseTreeKind::Path { segment, alias, sub_tree } => {
                                    assert_eq!(segment.0, "io");
                                    assert!(alias.is_none());
                                    assert!(sub_tree.is_none());
                                },
                                _ => panic!("Expected path"),
                            }
                            match &trees[1].kind {
                                UseTreeKind::Path { segment, alias, sub_tree } => {
                                    assert_eq!(segment.0, "fs");
                                    assert!(alias.is_none());
                                    assert!(sub_tree.is_none());
                                },
                                _ => panic!("Expected path"),
                            }
                        },
                        _ => panic!("Expected group"),
                    }
                },
                _ => panic!("Expected path"),
            }
        }

        #[test]
        fn test_parse_use_nested_group() {
            let use_decl = parse_use_decl("use std::{io::{Read, Write}, fs::{File, OpenOptions}};").unwrap();
            match use_decl.tree.kind {
                UseTreeKind::Path { segment, alias, sub_tree } => {
                    assert_eq!(segment.0, "std");
                    assert!(alias.is_none());
                    assert!(sub_tree.is_some());
                    let sub_tree = sub_tree.unwrap();
                    match sub_tree.kind {
                        UseTreeKind::Group(trees) => {
                            assert_eq!(trees.len(), 2);
                            // Check io group
                            match &trees[0].kind {
                                UseTreeKind::Path { segment, alias, sub_tree } => {
                                    assert_eq!(segment.0, "io");
                                    assert!(alias.is_none());
                                    assert!(sub_tree.is_some());
                                    let sub_tree = sub_tree.as_ref().unwrap();
                                    match &sub_tree.kind {
                                        UseTreeKind::Group(io_trees) => {
                                            assert_eq!(io_trees.len(), 2);
                                            assert_eq!(match &io_trees[0].kind {
                                                UseTreeKind::Path { segment, .. } => &segment.0,
                                                _ => panic!("Expected path"),
                                            }, "Read");
                                            assert_eq!(match &io_trees[1].kind {
                                                UseTreeKind::Path { segment, .. } => &segment.0,
                                                _ => panic!("Expected path"),
                                            }, "Write");
                                        },
                                        _ => panic!("Expected group"),
                                    }
                                },
                                _ => panic!("Expected path"),
                            }
                            // Check fs group
                            match &trees[1].kind {
                                UseTreeKind::Path { segment, alias, sub_tree } => {
                                    assert_eq!(segment.0, "fs");
                                    assert!(alias.is_none());
                                    assert!(sub_tree.is_some());
                                    let sub_tree = sub_tree.as_ref().unwrap();
                                    match &sub_tree.kind {
                                        UseTreeKind::Group(fs_trees) => {
                                            assert_eq!(fs_trees.len(), 2);
                                            assert_eq!(match &fs_trees[0].kind {
                                                UseTreeKind::Path { segment, .. } => &segment.0,
                                                _ => panic!("Expected path"),
                                            }, "File");
                                            assert_eq!(match &fs_trees[1].kind {
                                                UseTreeKind::Path { segment, .. } => &segment.0,
                                                _ => panic!("Expected path"),
                                            }, "OpenOptions");
                                        },
                                        _ => panic!("Expected group"),
                                    }
                                },
                                _ => panic!("Expected path"),
                            }
                        },
                        _ => panic!("Expected group"),
                    }
                },
                _ => panic!("Expected path"),
            }
        }

        #[test]
        fn test_parse_use_mixed_group() {
            let use_decl = parse_use_decl("use std::io::{self, Read as IoRead, Write, *};").unwrap();
            match use_decl.tree.kind {
                UseTreeKind::Path { segment, alias, sub_tree } => {
                    assert_eq!(segment.0, "std");
                    assert!(alias.is_none());
                    assert!(sub_tree.is_some());
                    let sub_tree = sub_tree.unwrap();
                    match sub_tree.kind {
                        UseTreeKind::Path { segment, alias, sub_tree } => {
                            assert_eq!(segment.0, "io");
                            assert!(alias.is_none());
                            assert!(sub_tree.is_some());
                            let sub_tree = sub_tree.unwrap();
                            match sub_tree.kind {
                                UseTreeKind::Group(trees) => {
                                    assert_eq!(trees.len(), 4);
                                    // Check self
                                    match &trees[0].kind {
                                        UseTreeKind::Path { segment, alias, sub_tree } => {
                                            assert_eq!(segment.0, "self");
                                            assert!(alias.is_none());
                                            assert!(sub_tree.is_none());
                                        },
                                        _ => panic!("Expected path"),
                                    }
                                    // Check Read with alias
                                    match &trees[1].kind {
                                        UseTreeKind::Path { segment, alias, sub_tree } => {
                                            assert_eq!(segment.0, "Read");
                                            assert_eq!(alias.as_ref().unwrap().0, "IoRead");
                                            assert!(sub_tree.is_none());
                                        },
                                        _ => panic!("Expected path"),
                                    }
                                    // Check Write
                                    match &trees[2].kind {
                                        UseTreeKind::Path { segment, alias, sub_tree } => {
                                            assert_eq!(segment.0, "Write");
                                            assert!(alias.is_none());
                                            assert!(sub_tree.is_none());
                                        },
                                        _ => panic!("Expected path"),
                                    }
                                    // Check glob
                                    match &trees[3].kind {
                                        UseTreeKind::Glob => {},
                                        _ => panic!("Expected glob"),
                                    }
                                },
                                _ => panic!("Expected group"),
                            }
                        },
                        _ => panic!("Expected path"),
                    }
                },
                _ => panic!("Expected path"),
            }
        }
    }

    mod function_tests {
        use super::*;
        use crate::ast::expr::{ExprKind, BinaryOp};
        use crate::ast::pattern::PatternKind;

        #[test]
        fn test_simple_function() {
            let item = parse_item_from_source(r#"
                fn add(x: i32, y: i32) -> i32 = x + y;
            "#).unwrap();
            match item.kind {
                ItemKind::Function(func) => {
                    assert_eq!(func.name.0, "add");
                    assert!(func.generic_params.is_none());
                    assert!(func.where_clause.is_none());
                    // Check parameters
                    assert_eq!(func.params.len(), 2);
                    let x = &func.params[0];
                    match &x.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "x"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &x.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                        _ => panic!("Expected path type"),
                    }
                    // Check return type
                    match &func.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                        _ => panic!("Expected path type"),
                    }
                    // Check body
                    match &func.body.kind {
                        ExprKind::Binary { left, op, right } => {
                            assert_eq!(*op, BinaryOp::Add);
                            match &left.kind {
                                ExprKind::Path(path) => assert_eq!(path[0].0, "x"),
                                _ => panic!("Expected path expression"),
                            }
                            match &right.kind {
                                ExprKind::Path(path) => assert_eq!(path[0].0, "y"),
                                _ => panic!("Expected path expression"),
                            }
                        },
                        _ => panic!("Expected binary expression"),
                    }
                },
                _ => panic!("Expected function"),
            }
        }

        #[test]
        fn test_generic_function() {
            let item = parse_item_from_source(r#"
                fn map<T, U>(x: T, f: fn(T) -> U) -> U = f(x);
            "#).unwrap();
            match item.kind {
                ItemKind::Function(func) => {
                    assert_eq!(func.name.0, "map");
                    // Check generic parameters
                    let generic_params = func.generic_params.as_ref().unwrap();
                    assert_eq!(generic_params.len(), 2);
                    assert_eq!(generic_params[0].name.0, "T");
                    assert_eq!(generic_params[1].name.0, "U");
                    // Check parameters
                    assert_eq!(func.params.len(), 2);
                    let x = &func.params[0];
                    match &x.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "x"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &x.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                    // Check return type
                    match &func.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "U"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected function"),
            }
        }

        #[test]
        fn test_pub_function() {
            let item = parse_item_from_source(r#"
                pub fn greet(name: String) -> String = format("Hello, {}!", name);
            "#).unwrap();
            match item.kind {
                ItemKind::Function(func) => {
                    assert!(item.visibility);
                    assert_eq!(func.name.0, "greet");
                    // Check parameters
                    assert_eq!(func.params.len(), 1);
                    let name = &func.params[0];
                    match &name.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "name"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &name.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected function"),
            }
        }

        #[test]
        fn test_function_with_default_param() {
            let item = parse_item_from_source(r#"
                fn configure(name: String, description: Option<String> = None) -> Config = Config::new(name, description);
            "#).unwrap();
            match item.kind {
                ItemKind::Function(func) => {
                    assert_eq!(func.name.0, "configure");
                    // Check parameters
                    assert_eq!(func.params.len(), 2);
                    let description = &func.params[1];
                    match &description.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "description"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    assert!(description.default_value.is_some());
                    match &description.default_value.as_ref().unwrap().kind {
                        ExprKind::Path(path) => assert_eq!(path[0].0, "None"),
                        _ => panic!("Expected path expression"),
                    }
                },
                _ => panic!("Expected function"),
            }
        }
    }

    mod trait_tests {
        use super::*;
        use crate::ast::expr::ExprKind;
        use crate::ast::pattern::PatternKind;

        #[test]
        fn test_simple_trait() {
            let item = parse_item_from_source(r#"
                trait Display {
                    fn fmt(self) -> String = self.to_string();
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Trait(trait_def) => {
                    assert_eq!(trait_def.name.0, "Display");
                    assert!(trait_def.generic_params.is_none());
                    assert!(trait_def.where_clause.is_none());
                    assert!(trait_def.supertraits.is_empty());
                    assert_eq!(trait_def.items.len(), 1);
                    // Check method
                    let method = &trait_def.items[0].function;
                    assert_eq!(method.name.0, "fmt");
                    assert_eq!(method.params.len(), 1);
                    let self_param = &method.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &method.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(trait_def.items[0].default_impl.is_some());
                },
                _ => panic!("Expected trait definition"),
            }
        }

        #[test]
        fn test_generic_trait() {
            let item = parse_item_from_source(r#"
                trait Container<T> {
                    fn insert(&mut self, item: T) = self.items.push(item);
                    fn contains(&self, item: &T) -> bool = self.items.contains(item);
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Trait(trait_def) => {
                    assert_eq!(trait_def.name.0, "Container");
                    // Check generic parameters
                    let generic_params = trait_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 1);
                    assert_eq!(generic_params[0].name.0, "T");
                    assert!(trait_def.where_clause.is_none());
                    assert!(trait_def.supertraits.is_empty());
                    assert_eq!(trait_def.items.len(), 2);
                    // Check insert method
                    let insert = &trait_def.items[0].function;
                    assert_eq!(insert.name.0, "insert");
                    assert_eq!(insert.params.len(), 2);
                    let self_param = &insert.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    let item_param = &insert.params[1];
                    match &item_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "item"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &item_param.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(insert.return_type.is_none());
                    // Check contains method
                    let contains = &trait_def.items[1].function;
                    assert_eq!(contains.name.0, "contains");
                    assert_eq!(contains.params.len(), 2);
                    let self_param = &contains.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    let item_param = &contains.params[1];
                    match &item_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "item"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &contains.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "bool"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected trait definition"),
            }
        }

        #[test]
        fn test_trait_with_supertraits() {
            let item = parse_item_from_source(r#"
                trait Debug: Display + Clone {
                    fn debug(&self) -> String = self.to_string();
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Trait(trait_def) => {
                    assert_eq!(trait_def.name.0, "Debug");
                    assert!(trait_def.generic_params.is_none());
                    assert!(trait_def.where_clause.is_none());
                    // Check supertraits
                    assert_eq!(trait_def.supertraits.len(), 2);
                    match &trait_def.supertraits[0].kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Display"),
                        _ => panic!("Expected path type"),
                    }
                    match &trait_def.supertraits[1].kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Clone"),
                        _ => panic!("Expected path type"),
                    }
                    // Check method
                    assert_eq!(trait_def.items.len(), 1);
                    let debug = &trait_def.items[0].function;
                    assert_eq!(debug.name.0, "debug");
                    assert_eq!(debug.params.len(), 1);
                    match &debug.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected trait definition"),
            }
        }

        #[test]
        fn test_trait_with_default_impl() {
            let item = parse_item_from_source(r#"
                trait Default {
                    fn default() -> Self = Self::new();
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Trait(trait_def) => {
                    assert_eq!(trait_def.name.0, "Default");
                    assert!(trait_def.generic_params.is_none());
                    assert!(trait_def.where_clause.is_none());
                    assert!(trait_def.supertraits.is_empty());
                    assert_eq!(trait_def.items.len(), 1);
                    // Check method
                    let default = &trait_def.items[0].function;
                    assert_eq!(default.name.0, "default");
                    assert!(default.params.is_empty());
                    match &default.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Self"),
                        _ => panic!("Expected path type"),
                    }
                    // Check default implementation
                    assert!(trait_def.items[0].default_impl.is_some());
                    match &trait_def.items[0].default_impl.as_ref().unwrap().kind {
                        ExprKind::Call { func, args } => {
                            match &func.kind {
                                ExprKind::Path(path) => {
                                    assert_eq!(path.len(), 2);
                                    assert_eq!(path[0].0, "Self");
                                    assert_eq!(path[1].0, "new");
                                },
                                _ => panic!("Expected path expression"),
                            }
                            assert!(args.is_empty());
                        },
                        _ => panic!("Expected call expression"),
                    }
                },
                _ => panic!("Expected trait definition"),
            }
        }

        #[test]
        fn test_pub_trait() {
            let item = parse_item_from_source(r#"
                pub trait FromStr {
                    type Err;
                    fn from_str(s: &str) -> Result<Self, Self::Err> = str::parse(s);
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Trait(trait_def) => {
                    assert!(item.visibility);
                    assert_eq!(trait_def.name.0, "FromStr");
                    assert!(trait_def.generic_params.is_none());
                    assert!(trait_def.where_clause.is_none());
                    assert!(trait_def.supertraits.is_empty());
                    // TODO: Add assertions for associated type when supported
                    // Check method
                    assert_eq!(trait_def.items.len(), 1);
                    let from_str = &trait_def.items[0].function;
                    assert_eq!(from_str.name.0, "from_str");
                    assert_eq!(from_str.params.len(), 1);
                    let s_param = &from_str.params[0];
                    match &s_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "s"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &s_param.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "str"),
                        _ => panic!("Expected path type"),
                    }
                    match &from_str.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Result"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected trait definition"),
            }
        }

        #[test]
        fn test_simple_trait_no_default() {
            let item = parse_item_from_source(r#"
                trait Display {
                    fn fmt(&self) -> String;
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Trait(trait_def) => {
                    assert_eq!(trait_def.name.0, "Display");
                    assert!(trait_def.generic_params.is_none());
                    assert!(trait_def.where_clause.is_none());
                    assert!(trait_def.supertraits.is_empty());
                    assert_eq!(trait_def.items.len(), 1);
                    // Check method
                    let method = &trait_def.items[0].function;
                    assert_eq!(method.name.0, "fmt");
                    assert_eq!(method.params.len(), 1);
                    let self_param = &method.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &method.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(trait_def.items[0].default_impl.is_none());
                },
                _ => panic!("Expected trait definition"),
            }
        }

        #[test]
        fn test_generic_trait_no_default() {
            let item = parse_item_from_source(r#"
                trait Container<T> {
                    fn insert(&mut self, item: T);
                    fn contains(&self, item: &T) -> bool;
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Trait(trait_def) => {
                    assert_eq!(trait_def.name.0, "Container");
                    // Check generic parameters
                    let generic_params = trait_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 1);
                    assert_eq!(generic_params[0].name.0, "T");
                    assert!(trait_def.where_clause.is_none());
                    assert!(trait_def.supertraits.is_empty());
                    assert_eq!(trait_def.items.len(), 2);
                    // Check insert method
                    let insert = &trait_def.items[0].function;
                    assert_eq!(insert.name.0, "insert");
                    assert_eq!(insert.params.len(), 2);
                    let self_param = &insert.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    let item_param = &insert.params[1];
                    match &item_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "item"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &item_param.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(insert.return_type.is_none());
                    assert!(trait_def.items[0].default_impl.is_none());
                    // Check contains method
                    let contains = &trait_def.items[1].function;
                    assert_eq!(contains.name.0, "contains");
                    assert_eq!(contains.params.len(), 2);
                    let self_param = &contains.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    let item_param = &contains.params[1];
                    match &item_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "item"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &contains.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "bool"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(trait_def.items[1].default_impl.is_none());
                },
                _ => panic!("Expected trait definition"),
            }
        }

        #[test]
        fn test_trait_mixed_default_and_required() {
            let item = parse_item_from_source(r#"
                trait Iterator {
                    type Item;
                    fn next(&mut self) -> Option<Self::Item>;
                    fn size_hint(&self) -> (usize, Option<usize>) = (0, None);
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Trait(trait_def) => {
                    assert_eq!(trait_def.name.0, "Iterator");
                    assert!(trait_def.generic_params.is_none());
                    assert!(trait_def.where_clause.is_none());
                    assert!(trait_def.supertraits.is_empty());
                    // TODO: Add assertions for associated type when supported
                    assert_eq!(trait_def.items.len(), 2);
                    // Check next method (required)
                    let next = &trait_def.items[0].function;
                    assert_eq!(next.name.0, "next");
                    assert_eq!(next.params.len(), 1);
                    let self_param = &next.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &next.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Option"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(trait_def.items[0].default_impl.is_none());
                    // Check size_hint method (with default)
                    let size_hint = &trait_def.items[1].function;
                    assert_eq!(size_hint.name.0, "size_hint");
                    assert_eq!(size_hint.params.len(), 1);
                    let self_param = &size_hint.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &size_hint.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "usize"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(trait_def.items[1].default_impl.is_some());
                },
                _ => panic!("Expected trait definition"),
            }
        }
    }

    mod impl_tests {
        use super::*;
        use crate::ast::pattern::PatternKind;

        #[test]
        fn test_simple_impl() {
            let item = parse_item_from_source(r#"
                impl Point {
                    fn new(x: f64, y: f64) -> Self {
                        Point { x, y }
                    }

                    fn distance(&self, other: &Point) -> f64 {
                        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
                    }
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Impl(impl_def) => {
                    assert!(impl_def.trait_type.is_none());
                    assert!(impl_def.generic_params.is_none());
                    assert!(impl_def.where_clause.is_none());
                    // Check self type
                    match &impl_def.self_type.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Point"),
                        _ => panic!("Expected path type"),
                    }
                    // Check methods
                    assert_eq!(impl_def.items.len(), 2);
                    // Check new method
                    let new = &impl_def.items[0];
                    assert_eq!(new.name.0, "new");
                    assert_eq!(new.params.len(), 2);
                    let x = &new.params[0];
                    match &x.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "x"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &x.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                        _ => panic!("Expected path type"),
                    }
                    let y = &new.params[1];
                    match &y.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "y"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &y.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                        _ => panic!("Expected path type"),
                    }
                    match &new.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Self"),
                        _ => panic!("Expected path type"),
                    }
                    // Check distance method
                    let distance = &impl_def.items[1];
                    assert_eq!(distance.name.0, "distance");
                    assert_eq!(distance.params.len(), 2);
                    let self_param = &distance.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    let other = &distance.params[1];
                    match &other.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "other"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &distance.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected impl block"),
            }
        }

        #[test]
        fn test_trait_impl() {
            let item = parse_item_from_source(r#"
                impl Display for Point {
                    fn fmt(self) -> String = {
                        format("Point({}, {})", self.x, self.y)
                    }
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Impl(impl_def) => {
                    // Check trait type
                    let trait_type = impl_def.trait_type.as_ref().unwrap();
                    match &trait_type.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Display"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(impl_def.generic_params.is_none());
                    assert!(impl_def.where_clause.is_none());
                    // Check self type
                    match &impl_def.self_type.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Point"),
                        _ => panic!("Expected path type"),
                    }
                    // Check method
                    assert_eq!(impl_def.items.len(), 1);
                    let fmt = &impl_def.items[0];
                    assert_eq!(fmt.name.0, "fmt");
                    assert_eq!(fmt.params.len(), 1);
                    let self_param = &fmt.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &fmt.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected impl block"),
            }
        }

        #[test]
        fn test_generic_impl() {
            let item = parse_item_from_source(r#"
                impl<T> Container<T> {
                    fn new() -> Self {
                        Container { items: Vec::new() }
                    }

                    fn add(&mut self, item: T) {
                        self.items.push(item);
                    }
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Impl(impl_def) => {
                    assert!(impl_def.trait_type.is_none());
                    // Check generic parameters
                    let generic_params = impl_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 1);
                    assert_eq!(generic_params[0].name.0, "T");
                    assert!(impl_def.where_clause.is_none());
                    // Check self type
                    match &impl_def.self_type.kind {
                        TypeKind::Path(path) => {
                            assert_eq!(path[0].0, "Container");
                            // TODO: Check generic argument T
                        },
                        _ => panic!("Expected path type"),
                    }
                    // Check methods
                    assert_eq!(impl_def.items.len(), 2);
                    // Check new method
                    let new = &impl_def.items[0];
                    assert_eq!(new.name.0, "new");
                    assert!(new.params.is_empty());
                    match &new.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Self"),
                        _ => panic!("Expected path type"),
                    }
                    // Check add method
                    let add = &impl_def.items[1];
                    assert_eq!(add.name.0, "add");
                    assert_eq!(add.params.len(), 2);
                    let self_param = &add.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    let item_param = &add.params[1];
                    match &item_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "item"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &item_param.ty.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                    assert!(add.return_type.is_none());
                },
                _ => panic!("Expected impl block"),
            }
        }

        #[test]
        fn test_impl_with_where_clause() {
            let item = parse_item_from_source(r#"
                impl<T> ToString for T where T: Display {
                    fn to_string(&self) -> String {
                        self.fmt()
                    }
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Impl(impl_def) => {
                    // Check trait type
                    let trait_type = impl_def.trait_type.as_ref().unwrap();
                    match &trait_type.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "ToString"),
                        _ => panic!("Expected path type"),
                    }
                    // Check generic parameters
                    let generic_params = impl_def.generic_params.unwrap();
                    assert_eq!(generic_params.len(), 1);
                    assert_eq!(generic_params[0].name.0, "T");
                    // Check where clause
                    let where_clause = impl_def.where_clause.unwrap();
                    assert_eq!(where_clause.predicates.len(), 1);
                    let pred = &where_clause.predicates[0];
                    match &pred.ty.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                    match &pred.bounds[0].kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "Display"),
                        _ => panic!("Expected path type"),
                    }
                    // Check self type
                    match &impl_def.self_type.kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                        _ => panic!("Expected path type"),
                    }
                    // Check method
                    assert_eq!(impl_def.items.len(), 1);
                    let to_string = &impl_def.items[0];
                    assert_eq!(to_string.name.0, "to_string");
                    assert_eq!(to_string.params.len(), 1);
                    let self_param = &to_string.params[0];
                    match &self_param.pattern.kind {
                        PatternKind::Identifier(ident) => assert_eq!(ident.0, "self"),
                        _ => panic!("Expected identifier pattern"),
                    }
                    match &to_string.return_type.as_ref().unwrap().kind {
                        TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                        _ => panic!("Expected path type"),
                    }
                },
                _ => panic!("Expected impl block"),
            }
        }
    }

    mod module_tests {
        use super::*;

        #[test]
        fn test_empty_module() {
            let source = "mod test {}";
            println!("\nTesting empty module with source: {}", source);
            
            let mut parser = create_test_parser();
            let tree = parser.parse(source, None).unwrap();
            println!("\nFull AST:");
            print_test_tree(&tree.root_node(), source, 10);
            
            // First find the item node, then find the module within it
            if let Some(item_node) = find_first_child(&tree.root_node(), "item") {
                println!("\nFound item node: {}", item_node.to_sexp());
                if let Some(module_node) = find_first_child(&item_node, "module") {
                    println!("\nFound module node: {}", module_node.to_sexp());
                    let module = parse_module(&module_node, source).unwrap();
                    println!("\nParsed module:");
                    println!("  name: {}", module.name.0);
                    println!("  items: {}", module.items.len());
                    println!("  span: {}..{}", module.span.start, module.span.end);
                    
                    // Verify the parsed module
                    assert_eq!(module.name.0, "test");
                    assert_eq!(module.items.len(), 0);
                    assert_eq!(module.span.start, 0);
                    assert_eq!(module.span.end, 11);
                } else {
                    panic!("Could not find module node within item");
                }
            } else {
                panic!("Could not find item node");
            }
        }

        #[test]
        fn test_module_with_items() {
            let item = parse_item_from_source(r#"
                mod math {
                    struct Point {
                        x: f64,
                        y: f64,
                    }

                    fn distance(p1: &Point, p2: &Point) -> f64 {
                        ((p2.x - p1.x).powi(2) + (p2.y - p1.y).powi(2)).sqrt()
                    }
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Module(module) => {
                    assert_eq!(module.name.0, "math");
                    assert_eq!(module.items.len(), 2);
                    // Check struct
                    match &module.items[0].kind {
                        ItemKind::Struct(struct_def) => {
                            assert_eq!(struct_def.name.0, "Point");
                            assert_eq!(struct_def.fields.len(), 2);
                            assert_eq!(struct_def.fields[0].name.0, "x");
                            match &struct_def.fields[0].ty.kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                                _ => panic!("Expected path type"),
                            }
                            assert_eq!(struct_def.fields[1].name.0, "y");
                            match &struct_def.fields[1].ty.kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected struct"),
                    }
                    // Check function
                    match &module.items[1].kind {
                        ItemKind::Function(func) => {
                            assert_eq!(func.name.0, "distance");
                            assert_eq!(func.params.len(), 2);
                            match &func.return_type.as_ref().unwrap().kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "f64"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected function"),
                    }
                },
                _ => panic!("Expected module"),
            }
        }

        #[test]
        fn test_nested_modules() {
            let item = parse_item_from_source(r#"
                mod outer {
                    mod inner {
                        fn nested_function() -> i32 {
                            42
                        }
                    }

                    fn outer_function() -> bool {
                        true
                    }
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Module(outer) => {
                    assert_eq!(outer.name.0, "outer");
                    assert_eq!(outer.items.len(), 2);
                    // Check inner module
                    match &outer.items[0].kind {
                        ItemKind::Module(inner) => {
                            assert_eq!(inner.name.0, "inner");
                            assert_eq!(inner.items.len(), 1);
                            // Check nested function
                            match &inner.items[0].kind {
                                ItemKind::Function(func) => {
                                    assert_eq!(func.name.0, "nested_function");
                                    assert!(func.params.is_empty());
                                    match &func.return_type.as_ref().unwrap().kind {
                                        TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                                        _ => panic!("Expected path type"),
                                    }
                                },
                                _ => panic!("Expected function"),
                            }
                        },
                        _ => panic!("Expected module"),
                    }
                    // Check outer function
                    match &outer.items[1].kind {
                        ItemKind::Function(func) => {
                            assert_eq!(func.name.0, "outer_function");
                            assert!(func.params.is_empty());
                            match &func.return_type.as_ref().unwrap().kind {
                                TypeKind::Path(path) => assert_eq!(path[0].0, "bool"),
                                _ => panic!("Expected path type"),
                            }
                        },
                        _ => panic!("Expected function"),
                    }
                },
                _ => panic!("Expected module"),
            }
        }

        #[test]
        fn test_pub_module() {
            let item = parse_item_from_source(r#"
                pub mod utils {
                    pub fn public_function() = ();
                    fn private_function() = ();
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Module(module) => {
                    assert!(item.visibility);
                    assert_eq!(module.name.0, "utils");
                    assert_eq!(module.items.len(), 2);
                    // Check public function
                    match &module.items[0].kind {
                        ItemKind::Function(func) => {
                            assert!(module.items[0].visibility);
                            assert_eq!(func.name.0, "public_function");
                            assert!(func.params.is_empty());
                            assert!(func.return_type.is_none());
                        },
                        _ => panic!("Expected function"),
                    }
                    // Check private function
                    match &module.items[1].kind {
                        ItemKind::Function(func) => {
                            assert!(!module.items[1].visibility);
                            assert_eq!(func.name.0, "private_function");
                            assert!(func.params.is_empty());
                            assert!(func.return_type.is_none());
                        },
                        _ => panic!("Expected function"),
                    }
                },
                _ => panic!("Expected module"),
            }
        }

        #[test]
        fn test_module_with_use_declarations() {
            let item = parse_item_from_source(r#"
                mod graphics {
                    use std::fmt::Display;
                    use crate::math::{Point, Vector};

                    struct Color(u8, u8, u8);
                }
            "#).unwrap();
            match item.kind {
                ItemKind::Module(module) => {
                    assert_eq!(module.name.0, "graphics");
                    assert_eq!(module.items.len(), 3);
                    // Check first use declaration
                    match &module.items[0].kind {
                        ItemKind::Use(use_decl) => {
                            match &use_decl.tree.kind {
                                UseTreeKind::Path { segment, alias, sub_tree } => {
                                    assert_eq!(segment.0, "std");
                                    assert!(alias.is_none());
                                    assert!(sub_tree.is_some());
                                },
                                _ => panic!("Expected path"),
                            }
                        },
                        _ => panic!("Expected use declaration"),
                    }
                    // Check second use declaration
                    match &module.items[1].kind {
                        ItemKind::Use(use_decl) => {
                            match &use_decl.tree.kind {
                                UseTreeKind::Path { segment, alias, sub_tree } => {
                                    assert_eq!(segment.0, "crate");
                                    assert!(alias.is_none());
                                    assert!(sub_tree.is_some());
                                },
                                _ => panic!("Expected path"),
                            }
                        },
                        _ => panic!("Expected use declaration"),
                    }
                    // Check struct declaration
                    match &module.items[2].kind {
                        ItemKind::Struct(struct_def) => {
                            assert_eq!(struct_def.name.0, "Color");
                            assert_eq!(struct_def.fields.len(), 3);
                            for field in &struct_def.fields {
                                match &field.ty.kind {
                                    TypeKind::Path(path) => assert_eq!(path[0].0, "u8"),
                                    _ => panic!("Expected path type"),
                                }
                            }
                        },
                        _ => panic!("Expected struct"),
                    }
                },
                _ => panic!("Expected module"),
            }
        }
    }
} 