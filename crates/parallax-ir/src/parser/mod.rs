pub mod parser;

pub use parser::*;

#[cfg(test)]
mod tests {
    use crate::lexer::lex;
    use crate::ast::{Node, Numeric, Tree};
    use crate::parser::{parse_book, Parser};
    use crate::IRResult;
    use std::fs;
    use std::path::Path;

    #[test]
    fn test_parse_empty_book() -> IRResult<()> {
        let input = "";
        let tokens = lex(input)?;
        let book = parse_book(&tokens)?;
        assert!(book.definitions.is_empty());
        Ok(())
    }

    #[test]
    fn test_parse_single_definition() -> IRResult<()> {
        let input = "@main = (a b)";
        let tokens = lex(input)?;
        let book = parse_book(&tokens)?;
        assert_eq!(book.definitions.len(), 1);
        assert_eq!(book.definitions[0].name, "main");
        Ok(())
    }

    #[test]
    fn test_parse_example() -> IRResult<()> {
        let input = r#"
            @down = (?(((a (* a)) @down__C0) (b (c d))) (c (b d)))
        "#;
        let tokens = lex(input)?;
        let book = parse_book(&tokens)?;
        assert_eq!(book.definitions.len(), 1);
        assert_eq!(book.definitions[0].name, "down");
        Ok(())
    }

    #[test]
    fn test_parse_numeric() -> IRResult<()> {
        let input = "42";
        let tokens = lex(input)?;
        let mut parser = Parser::new(&tokens);
        let numeric = parser.parse_numeric()?;
        assert_eq!(numeric, Numeric::Number("42"));
        Ok(())
    }

    #[test]
    fn test_parse_operator() -> IRResult<()> {
        let input = "*";
        let tokens = lex(input)?;
        let mut parser = Parser::new(&tokens);
        let operator = parser.parse_operator()?;
        assert_eq!(operator, "*");
        Ok(())
    }

    #[test]
    fn test_parse_complex_tree() -> IRResult<()> {
        let input = "(a (b c))";
        let tokens = lex(input)?;
        let mut parser = Parser::new(&tokens);
        let tree = parser.parse_tree()?;

        if let Tree::Node(node) = tree {
            if let Node::Constructor(left, right) = node {
                assert!(matches!(*left, Tree::Var("a")));

                if let Tree::Node(inner_node) = *right {
                    if let Node::Constructor(inner_left, inner_right) = inner_node {
                        assert!(matches!(*inner_left, Tree::Var("b")));
                        assert!(matches!(*inner_right, Tree::Var("c")));
                    } else {
                        panic!("Expected constructor node for '(b c)'");
                    }
                } else {
                    panic!("Expected node for '(b c)'");
                }
            } else {
                panic!("Expected constructor node");
            }
        } else {
            panic!("Expected node");
        }

        Ok(())
    }

    #[test]
    fn test_parse_redex() -> IRResult<()> {
        let input = "a ~ b"; // Simple redex: variable ~ variable
        let tokens = lex(input)?;
        let mut parser = Parser::new(&tokens);
        let redex = parser.parse_redex()?;

        // Check that we have a redex with two variables
        assert!(matches!(redex.left, Tree::Var("a")));
        assert!(matches!(redex.right, Tree::Var("b")));

        // Test a more complex redex
        let input = "(a b) ~ (c d)"; // Constructor ~ Constructor
        let tokens = lex(input)?;
        let mut parser = Parser::new(&tokens);
        let redex = parser.parse_redex()?;

        // Check that both sides are nodes
        assert!(matches!(redex.left, Tree::Node(_)));
        assert!(matches!(redex.right, Tree::Node(_)));

        Ok(())
    }

    #[test]
    fn test_parse_error_handling() -> IRResult<()> {
        let input = "@invalid = (";
        let tokens = lex(input)?;
        let result = parse_book(&tokens);
        assert!(result.is_err());
        Ok(())
    }

    #[test]
    fn test_parse_sort_bitonic() -> IRResult<()> {
        let content = fs::read_to_string(Path::new("../../tests/sort_bitonic.hvm"))
            .expect("Failed to read file");
        let tokens = lex(&content)?;
        let book = parse_book(&tokens)?;

        // Verify specific definitions exist
        let def_names: Vec<&str> = book.definitions.iter().map(|d| d.name).collect();
        assert!(def_names.contains(&"Busy"));
        assert!(def_names.contains(&"Concat"));
        assert!(def_names.contains(&"Empty"));
        assert!(def_names.contains(&"Node"));
        assert!(def_names.contains(&"sort"));
        assert!(def_names.contains(&"merge"));

        // Check specific definition structures
        let main_def = book.definitions.iter().find(|d| d.name == "main").unwrap();
        assert_eq!(main_def.net.redexes.len(), 1); // main has one redex

        // Check merge definition which has complex structure
        let merge_def = book.definitions.iter().find(|d| d.name == "merge").unwrap();
        if let Tree::Node(Node::Constructor(_, _)) = merge_def.net.tree {
            // Correct structure
        } else {
            panic!("merge definition should have Constructor node structure");
        }

        Ok(())
    }

    #[test]
    fn test_parse_stress() -> IRResult<()> {
        let content = fs::read_to_string(Path::new("../../tests/stress.hvm"))
            .expect("Failed to read file");
        let tokens = lex(&content)?;
        let book = parse_book(&tokens)?;

        // Verify the stress test specific definitions
        let def_names: Vec<&str> = book.definitions.iter().map(|d| d.name).collect();
        assert!(def_names.contains(&"fun"));
        assert!(def_names.contains(&"loop"));
        assert!(def_names.contains(&"main"));

        // Check fun definition structure: (?((@fun__C0 @fun__C1) a) a)
        let fun_def = book.definitions.iter().find(|d| d.name == "fun").unwrap();
        if let Tree::Node(Node::Constructor(_, _)) = &fun_def.net.tree {
            // Correct structure - fun is a constructor containing a switch
        } else {
            panic!("fun definition should be a constructor node");
        }

        // Check loop definition structure: (?((0 @loop__C0) a) a)
        let loop_def = book.definitions.iter().find(|d| d.name == "loop").unwrap();
        if let Tree::Node(Node::Constructor(_, _)) = &loop_def.net.tree {
            // Correct structure - loop is a constructor containing a switch
        } else {
            panic!("loop definition should be a constructor node");
        }

        Ok(())
    }

    #[test]
    fn test_parse_sum_rec() -> IRResult<()> {
        let content = fs::read_to_string(Path::new("../../tests/sum_rec.hvm"))
            .expect("Failed to read file");
        let tokens = lex(&content)?;
        let book = parse_book(&tokens)?;

        // Verify the recursive sum definitions
        let def_names: Vec<&str> = book.definitions.iter().map(|d| d.name).collect();
        assert!(def_names.contains(&"main"));
        assert!(def_names.contains(&"sum"));

        // Check main definition calls sum with correct parameters
        let main_def = book.definitions.iter().find(|d| d.name == "main").unwrap();
        assert_eq!(main_def.net.redexes.len(), 1); // Should have one redex

        // Check sum__C0 definition which contains the recursive structure
        let sum_c0_def = book.definitions.iter().find(|d| d.name == "sum__C0").unwrap();
        assert!(sum_c0_def.net.redexes.len() >= 2); // Should have at least two redexes for recursion

        Ok(())
    }

    #[test]
    fn test_parse_sum_tree() -> IRResult<()> {
        let content = fs::read_to_string(Path::new("../../tests/sum_tree.hvm"))
            .expect("Failed to read file");
        let tokens = lex(&content)?;
        let book = parse_book(&tokens)?;

        // Verify the tree sum definitions
        let def_names: Vec<&str> = book.definitions.iter().map(|d| d.name).collect();
        assert!(def_names.contains(&"gen"));
        assert!(def_names.contains(&"main"));
        assert!(def_names.contains(&"sum"));

        // Check gen definition structure: (?(((a a) @gen__C0) b) b)
        let gen_def = book.definitions.iter().find(|d| d.name == "gen").unwrap();
        if let Tree::Node(Node::Constructor(_, _)) = &gen_def.net.tree {
            // Correct structure - gen is a constructor containing a switch
        } else {
            panic!("gen definition should be a constructor node");
        }

        // Check gen__C0 definition which contains the recursive structure
        let gen_c0_def = book.definitions.iter().find(|d| d.name == "gen__C0").unwrap();
        assert!(gen_c0_def.net.redexes.len() >= 2); // Should have at least two redexes for recursion

        Ok(())
    }

    #[test]
    fn test_parse_tuples() -> IRResult<()> {
        let content = fs::read_to_string(Path::new("../../tests/tuples.hvm"))
            .expect("Failed to read file");
        let tokens = lex(&content)?;
        let book = parse_book(&tokens)?;

        // Verify tuple-specific definitions
        let def_names: Vec<&str> = book.definitions.iter().map(|d| d.name).collect();
        assert!(def_names.contains(&"Tup8/New"));
        assert!(def_names.contains(&"app"));
        assert!(def_names.contains(&"main"));
        assert!(def_names.contains(&"rot"));

        // Check Tup8/New definition which creates 8-tuples
        let tup_def = book.definitions.iter().find(|d| d.name == "Tup8/New").unwrap();
        if let Tree::Node(Node::Constructor(_, _)) = tup_def.net.tree {
            // Correct structure - Tup8/New uses nested constructors
        } else {
            panic!("Tup8/New definition should have Constructor node structure");
        }

        // Verify rot__C0 definition has correct structure for tuple rotation
        let rot_c0_def = book.definitions.iter().find(|d| d.name == "rot__C0").unwrap();
        assert_eq!(rot_c0_def.net.redexes.len(), 1); // Should have one redex for the rotation operation

        Ok(())
    }
}
