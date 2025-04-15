use parallax_syntax::ParallaxParser;
use tree_sitter::Tree;

fn main() {
    // Create a parser instance
    let mut parser = ParallaxParser::new().expect("Failed to create parser");
    
    // Sample pattern matching code
    let sample_code = r#"
fn match_test(value: Option<(i32, Point)>) = {
    match value {
        Some((x, Point { y, .. })) => println("X and Y extracted"),
        Some((0 | 1, _)) => println("Zero or One"),
        Some((z, p)) => println("Specific tuple {} at {:?}", z, p),
        None => println("None"),
        _ => println("Other Some"),
    }
};
"#;
    
    // Parse the code
    let tree = parser.parse(sample_code).expect("Failed to parse code");
    
    // Print the tree structure
    println!("Tree structure for pattern matching example:");
    print_tree_structure(&tree, sample_code, 15);
}

// Function to print the tree structure
fn print_tree_structure(tree: &Tree, source: &str, max_depth: usize) {
    print_tree_structure_recursive(&tree.root_node(), source, 0, max_depth);
}

fn print_tree_structure_recursive(node: &tree_sitter::Node, source: &str, depth: usize, max_depth: usize) {
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
            print_tree_structure_recursive(&cursor.node(), source, depth + 1, max_depth);
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }
} 