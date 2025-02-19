use tree_sitter::Node;
use crate::error::ParallaxError;
use crate::ast::common::{Span, Ident};
use crate::ast::types::{Type, TypeKind};
use super::expr;
use super::common;

pub fn parse_type(node: &Node, source: &str) -> Result<Type, ParallaxError> {
    let span = common::create_span(node);

    let kind = match node.kind() {
        "type" => {
            // Get the first child which should be the actual type
            let type_node = node.child(0)
                .ok_or_else(|| common::node_error(node, "Type node has no children"))?;
            return parse_type(&type_node, source);
        },
        "path" => {
            let path = common::parse_path(node, source)?;
            TypeKind::Path(path)
        },
        "kind_app" => {
            // Parse the base type
            let base_node = node.child(0)
                .ok_or_else(|| common::node_error(node, "Kind application has no base type"))?;
            let base_type = parse_type(&base_node, source)?;
            
            // Parse the type arguments - they appear as direct children after the base type
            let mut args = Vec::new();
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                if child.kind() == "type" && !child.eq(&base_node) {
                    args.push(parse_type(&child, source)?);
                }
            }
            
            TypeKind::KindApp(Box::new(base_type), args)
        },
        "function_type" => {
            // Parse parameter types
            let mut param_types = Vec::new();
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                if child.kind() == "type" {
                    // Skip this type if it's part of the return type
                    if let Some(return_node) = common::get_child(node, "return") {
                        if child.eq(&return_node) {
                            continue;
                        }
                    }
                    param_types.push(parse_type(&child, source)?);
                }
            }
            
            // Parse return type (defaults to unit type if not specified)
            let return_type = if let Some(return_node) = common::get_child(node, "return") {
                parse_type(&return_node, source)?
            } else {
                Type::new(TypeKind::Tuple(vec![]), span.clone())
            };

            // If there's exactly one parameter type, use it directly
            // Otherwise, wrap multiple parameters in a tuple type
            let param_type = if param_types.len() == 1 {
                param_types.remove(0)
            } else {
                Type::new(TypeKind::Tuple(param_types), span.clone())
            };

            TypeKind::Function(Box::new(param_type), Box::new(return_type))
        },
        "tuple_type" => {
            let mut types = Vec::new();
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                if child.kind() == "type" {
                    types.push(parse_type(&child, source)?);
                }
            }
            
            TypeKind::Tuple(types)
        },
        "array_type" => {
            let element_type = common::require_child(node, "element", "array_type")
                .and_then(|n| parse_type(&n, source))?;
            
            let size_expr = common::require_child(node, "size", "array_type")
                .and_then(|n| expr::parse_expr(&n, source))?;

            // Parse size expression and evaluate it to a constant
            let size = match size_expr.kind {
                crate::ast::expr::ExprKind::Literal(crate::ast::common::Literal::Int(n)) => n as usize,
                _ => return Err(common::node_error(node, "Array size must be a constant integer")),
            };

            TypeKind::Array(Box::new(element_type), size)
        },
        _ => return Err(common::node_error(node, &format!("Unknown type kind: {}", node.kind()))),
    };

    Ok(Type::new(kind, span))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::common::test_utils::*;
    use crate::ast::types::TypeKind;

    fn test_type_node(source: &str) -> Result<Type, ParallaxError> {
        let mut parser = create_test_parser();
        let tree = parser.parse(source, None).unwrap();
        print_test_tree(&tree.root_node(), source, 10);
        
        let type_node = find_node(&tree.root_node(), "type", &[])
            .ok_or_else(|| ParallaxError::ParseError {
                message: "Could not find type node".to_string(),
                span: None,
            })?;
        
        parse_type(&type_node, source)
    }

    #[test]
    fn test_simple_path_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(param: i32) = ();")?;
        match ty.kind {
            TypeKind::Path(path) => {
                assert_eq!(path.len(), 1);
                assert_eq!(path[0].0, "i32");
            },
            _ => panic!("Expected path type"),
        }
        Ok(())
    }

    #[test]
    fn test_complex_path_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(param: std::collections::HashMap) = ();")?;
        match ty.kind {
            TypeKind::Path(path) => {
                assert_eq!(path.len(), 3);
                assert_eq!(path[0].0, "std");
                assert_eq!(path[1].0, "collections");
                assert_eq!(path[2].0, "HashMap");
            },
            _ => panic!("Expected path type"),
        }
        Ok(())
    }

    #[test]
    fn test_simple_function_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(f: fn(i32) -> String) = ();")?;
        match ty.kind {
            TypeKind::Function(param, ret) => {
                match param.kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                    _ => panic!("Expected path type for parameter"),
                }
                match ret.kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                    _ => panic!("Expected path type for return"),
                }
            },
            _ => panic!("Expected function type"),
        }
        Ok(())
    }

    #[test]
    fn test_multi_param_function_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(f: fn(i32, String) -> bool) = ();")?;
        match ty.kind {
            TypeKind::Function(param, ret) => {
                match param.kind {
                    TypeKind::Tuple(params) => {
                        assert_eq!(params.len(), 2);
                        match &params[0].kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                            _ => panic!("Expected path type for first parameter"),
                        }
                        match &params[1].kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                            _ => panic!("Expected path type for second parameter"),
                        }
                    },
                    _ => panic!("Expected tuple type for parameters"),
                }
                match ret.kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "bool"),
                    _ => panic!("Expected path type for return"),
                }
            },
            _ => panic!("Expected function type"),
        }
        Ok(())
    }

    #[test]
    fn test_no_param_function_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(f: fn() -> i32) = ();")?;
        match ty.kind {
            TypeKind::Function(param, ret) => {
                match param.kind {
                    TypeKind::Tuple(params) => assert_eq!(params.len(), 0),
                    _ => panic!("Expected empty tuple type for parameters"),
                }
                match ret.kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                    _ => panic!("Expected path type for return"),
                }
            },
            _ => panic!("Expected function type"),
        }
        Ok(())
    }

    #[test]
    fn test_no_return_function_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(f: fn(i32)) = ();")?;
        match ty.kind {
            TypeKind::Function(param, ret) => {
                match param.kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                    _ => panic!("Expected path type for parameter"),
                }
                match ret.kind {
                    TypeKind::Tuple(types) => assert_eq!(types.len(), 0),
                    _ => panic!("Expected unit type for return"),
                }
            },
            _ => panic!("Expected function type"),
        }
        Ok(())
    }

    #[test]
    fn test_nested_function_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(f: fn(fn(i32) -> String) -> bool) = ();")?;
        match ty.kind {
            TypeKind::Function(param, ret) => {
                match param.kind {
                    TypeKind::Function(inner_param, inner_ret) => {
                        match inner_param.kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                            _ => panic!("Expected path type for inner parameter"),
                        }
                        match inner_ret.kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                            _ => panic!("Expected path type for inner return"),
                        }
                    },
                    _ => panic!("Expected function type for parameter"),
                }
                match ret.kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "bool"),
                    _ => panic!("Expected path type for return"),
                }
            },
            _ => panic!("Expected function type"),
        }
        Ok(())
    }

    #[test]
    fn test_empty_tuple_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(t: ()) = ();")?;
        match ty.kind {
            TypeKind::Tuple(types) => {
                assert_eq!(types.len(), 0);
            },
            _ => panic!("Expected tuple type"),
        }
        Ok(())
    }

    #[test]
    fn test_single_element_tuple_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(t: (i32,)) = ();")?;
        match ty.kind {
            TypeKind::Tuple(types) => {
                assert_eq!(types.len(), 1);
                match &types[0].kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                    _ => panic!("Expected path type"),
                }
            },
            _ => panic!("Expected tuple type"),
        }
        Ok(())
    }

    #[test]
    fn test_multi_element_tuple_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(t: (i32, String, bool)) = ();")?;
        match ty.kind {
            TypeKind::Tuple(types) => {
                assert_eq!(types.len(), 3);
                match &types[0].kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                    _ => panic!("Expected path type"),
                }
                match &types[1].kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                    _ => panic!("Expected path type"),
                }
                match &types[2].kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "bool"),
                    _ => panic!("Expected path type"),
                }
            },
            _ => panic!("Expected tuple type"),
        }
        Ok(())
    }

    #[test]
    fn test_array_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(arr: [i32; 5]) = ();")?;
        match ty.kind {
            TypeKind::Array(element_type, size) => {
                assert_eq!(size, 5);
                match element_type.kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                    _ => panic!("Expected path type"),
                }
            },
            _ => panic!("Expected array type"),
        }
        Ok(())
    }

    #[test]
    fn test_nested_array_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(arr: [[i32; 3]; 2]) = ();")?;
        match ty.kind {
            TypeKind::Array(outer_type, outer_size) => {
                assert_eq!(outer_size, 2);
                match outer_type.kind {
                    TypeKind::Array(inner_type, inner_size) => {
                        assert_eq!(inner_size, 3);
                        match inner_type.kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                            _ => panic!("Expected path type"),
                        }
                    },
                    _ => panic!("Expected array type"),
                }
            },
            _ => panic!("Expected array type"),
        }
        Ok(())
    }

    #[test]
    fn test_simple_kind_app() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(x: Option<i32>) = ();")?;
        match ty.kind {
            TypeKind::KindApp(base, args) => {
                match &base.kind {
                    TypeKind::Path(path) => {
                        assert_eq!(path.len(), 1, "Expected single path segment for Option");
                        assert_eq!(path[0].0, "Option", "Expected path to be Option");
                    },
                    _ => panic!("Expected path type for base"),
                }
                assert_eq!(args.len(), 1, "Expected exactly one type argument");
                match &args[0].kind {
                    TypeKind::Path(path) => {
                        assert_eq!(path.len(), 1, "Expected single path segment for i32");
                        assert_eq!(path[0].0, "i32", "Expected path to be i32");
                    },
                    _ => panic!("Expected path type for argument"),
                }
            },
            _ => panic!("Expected kind application"),
        }
        Ok(())
    }

    #[test]
    fn test_complex_kind_app() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(x: Result<Vec<i32>, Error>) = ();")?;
        match ty.kind {
            TypeKind::KindApp(base, args) => {
                match &base.kind {
                    TypeKind::Path(path) => {
                        assert_eq!(path.len(), 1, "Expected single path segment for Result");
                        assert_eq!(path[0].0, "Result", "Expected path to be Result");
                    },
                    _ => panic!("Expected path type for base"),
                }
                assert_eq!(args.len(), 2, "Expected exactly two type arguments");
                match &args[0].kind {
                    TypeKind::KindApp(inner_base, inner_args) => {
                        match &inner_base.kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for Vec");
                                assert_eq!(path[0].0, "Vec", "Expected path to be Vec");
                            },
                            _ => panic!("Expected path type for inner base"),
                        }
                        assert_eq!(inner_args.len(), 1, "Expected exactly one inner type argument");
                        match &inner_args[0].kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for i32");
                                assert_eq!(path[0].0, "i32", "Expected path to be i32");
                            },
                            _ => panic!("Expected path type for inner argument"),
                        }
                    },
                    _ => panic!("Expected kind application for first argument"),
                }
                match &args[1].kind {
                    TypeKind::Path(path) => {
                        assert_eq!(path.len(), 1, "Expected single path segment for Error");
                        assert_eq!(path[0].0, "Error", "Expected path to be Error");
                    },
                    _ => panic!("Expected path type for second argument"),
                }
            },
            _ => panic!("Expected kind application"),
        }
        Ok(())
    }

    #[test]
    fn test_mixed_complex_type() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(x: Result<(fn(i32) -> String, [bool; 4]), Vec<Error>>) = ();")?;
        match ty.kind {
            TypeKind::KindApp(base, args) => {
                // Check Result base
                match &base.kind {
                    TypeKind::Path(path) => assert_eq!(path[0].0, "Result"),
                    _ => panic!("Expected path type for base"),
                }
                assert_eq!(args.len(), 2);

                // Check first argument (tuple)
                match &args[0].kind {
                    TypeKind::Tuple(tuple_types) => {
                        assert_eq!(tuple_types.len(), 2);
                        
                        // Check function type
                        match &tuple_types[0].kind {
                            TypeKind::Function(param, ret) => {
                                match &param.kind {
                                    TypeKind::Path(path) => assert_eq!(path[0].0, "i32"),
                                    _ => panic!("Expected path type for function parameter"),
                                }
                                match &ret.kind {
                                    TypeKind::Path(path) => assert_eq!(path[0].0, "String"),
                                    _ => panic!("Expected path type for function return"),
                                }
                            },
                            _ => panic!("Expected function type in tuple"),
                        }

                        // Check array type
                        match &tuple_types[1].kind {
                            TypeKind::Array(element_type, size) => {
                                assert_eq!(*size, 4);
                                match &element_type.kind {
                                    TypeKind::Path(path) => assert_eq!(path[0].0, "bool"),
                                    _ => panic!("Expected path type for array element"),
                                }
                            },
                            _ => panic!("Expected array type in tuple"),
                        }
                    },
                    _ => panic!("Expected tuple type for first argument"),
                }

                // Check second argument (Vec<Error>)
                match &args[1].kind {
                    TypeKind::KindApp(vec_base, vec_args) => {
                        match &vec_base.kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "Vec"),
                            _ => panic!("Expected path type for Vec"),
                        }
                        assert_eq!(vec_args.len(), 1);
                        match &vec_args[0].kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "Error"),
                            _ => panic!("Expected path type for Vec argument"),
                        }
                    },
                    _ => panic!("Expected kind application for second argument"),
                }
            },
            _ => panic!("Expected kind application"),
        }
        Ok(())
    }

    #[test]
    fn test_function_with_generic_return() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(f: fn() -> Result<T, E>) = ();")?;
        match ty.kind {
            TypeKind::Function(param, ret) => {
                match param.kind {
                    TypeKind::Tuple(params) => assert_eq!(params.len(), 0),
                    _ => panic!("Expected empty tuple type for parameters"),
                }
                match ret.kind {
                    TypeKind::KindApp(base, args) => {
                        match &base.kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "Result"),
                            _ => panic!("Expected path type for base"),
                        }
                        assert_eq!(args.len(), 2);
                        match &args[0].kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                            _ => panic!("Expected path type for first type argument"),
                        }
                        match &args[1].kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "E"),
                            _ => panic!("Expected path type for second type argument"),
                        }
                    },
                    _ => panic!("Expected kind application for return type"),
                }
            },
            _ => panic!("Expected function type"),
        }
        Ok(())
    }

    #[test]
    fn test_nested_function_with_generic() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(f: fn(Option<T>) -> Vec<U>) = ();")?;
        match ty.kind {
            TypeKind::Function(param, ret) => {
                // Check parameter is Option<T>
                match param.kind {
                    TypeKind::KindApp(base, args) => {
                        match &base.kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "Option"),
                            _ => panic!("Expected path type for parameter base"),
                        }
                        assert_eq!(args.len(), 1);
                        match &args[0].kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "T"),
                            _ => panic!("Expected path type for parameter type argument"),
                        }
                    },
                    _ => panic!("Expected kind application for parameter"),
                }
                // Check return type is Vec<U>
                match ret.kind {
                    TypeKind::KindApp(base, args) => {
                        match &base.kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "Vec"),
                            _ => panic!("Expected path type for return base"),
                        }
                        assert_eq!(args.len(), 1);
                        match &args[0].kind {
                            TypeKind::Path(path) => assert_eq!(path[0].0, "U"),
                            _ => panic!("Expected path type for return type argument"),
                        }
                    },
                    _ => panic!("Expected kind application for return type"),
                }
            },
            _ => panic!("Expected function type"),
        }
        Ok(())
    }

    #[test]
    fn test_nested_kind_app() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(x: Vec<Result<T, E>>) = ();")?;
        match ty.kind {
            TypeKind::KindApp(base, args) => {
                match &base.kind {
                    TypeKind::Path(path) => {
                        assert_eq!(path.len(), 1, "Expected single path segment for Vec");
                        assert_eq!(path[0].0, "Vec", "Expected path to be Vec");
                    },
                    _ => panic!("Expected path type for base"),
                }
                assert_eq!(args.len(), 1, "Expected exactly one type argument");
                match &args[0].kind {
                    TypeKind::KindApp(inner_base, inner_args) => {
                        match &inner_base.kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for Result");
                                assert_eq!(path[0].0, "Result", "Expected path to be Result");
                            },
                            _ => panic!("Expected path type for inner base"),
                        }
                        assert_eq!(inner_args.len(), 2, "Expected exactly two inner type arguments");
                        match &inner_args[0].kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for T");
                                assert_eq!(path[0].0, "T", "Expected path to be T");
                            },
                            _ => panic!("Expected path type for first inner argument"),
                        }
                        match &inner_args[1].kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for E");
                                assert_eq!(path[0].0, "E", "Expected path to be E");
                            },
                            _ => panic!("Expected path type for second inner argument"),
                        }
                    },
                    _ => panic!("Expected kind application for argument"),
                }
            },
            _ => panic!("Expected kind application"),
        }
        Ok(())
    }

    #[test]
    fn test_function_with_complex_param() -> Result<(), ParallaxError> {
        let ty = test_type_node("fn main(f: fn(Result<T, E>) -> Vec<U>) = ();")?;
        match ty.kind {
            TypeKind::Function(param, ret) => {
                // Check parameter is Result<T, E>
                match param.kind {
                    TypeKind::KindApp(base, args) => {
                        match &base.kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for Result");
                                assert_eq!(path[0].0, "Result", "Expected path to be Result");
                            },
                            _ => panic!("Expected path type for parameter base"),
                        }
                        assert_eq!(args.len(), 2, "Expected exactly two type arguments");
                        match &args[0].kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for T");
                                assert_eq!(path[0].0, "T", "Expected path to be T");
                            },
                            _ => panic!("Expected path type for first type argument"),
                        }
                        match &args[1].kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for E");
                                assert_eq!(path[0].0, "E", "Expected path to be E");
                            },
                            _ => panic!("Expected path type for second type argument"),
                        }
                    },
                    _ => panic!("Expected kind application for parameter"),
                }
                // Check return type is Vec<U>
                match ret.kind {
                    TypeKind::KindApp(base, args) => {
                        match &base.kind {
                            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for Vec");
                                assert_eq!(path[0].0, "Vec", "Expected path to be Vec");
                            },
                            _ => panic!("Expected path type for return base"),
                        }
                        assert_eq!(args.len(), 1, "Expected exactly one type argument");
                        match &args[0].kind {
            TypeKind::Path(path) => {
                                assert_eq!(path.len(), 1, "Expected single path segment for U");
                                assert_eq!(path[0].0, "U", "Expected path to be U");
                            },
                            _ => panic!("Expected path type for return type argument"),
                        }
                    },
                    _ => panic!("Expected kind application for return type"),
                }
            },
            _ => panic!("Expected function type"),
        }
        Ok(())
    }
} 