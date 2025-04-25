use parallax_syntax::ParallaxParser;
use std::env;
use std::fs;
use std::process;

fn main() {
    // Get command line arguments
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        println!("Usage: {} <file_path> [max_depth]", args[0]);
        process::exit(1);
    }
    
    // Parse arguments
    let file_path = &args[1];
    let max_depth = if args.len() > 2 {
        args[2].parse::<usize>().unwrap_or(20) // Default depth is 20 if parsing fails
    } else {
        20 // Default depth
    };
    
    // Read the file
    let source = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            println!("Error reading file {}: {}", file_path, e);
            process::exit(1);
        }
    };
    
    // Create parser
    let mut parser = match ParallaxParser::new() {
        Ok(p) => p,
        Err(e) => {
            println!("Error creating parser: {}", e);
            process::exit(1);
        }
    };
    
    // Parse the source
    let tree = match parser.parse(&source) {
        Ok(t) => t,
        Err(e) => {
            println!("Error parsing file: {}", e);
            process::exit(1);
        }
    };
    
    // Print tree structure
    println!("Tree structure for {}:", file_path);
    println!("{}", "=".repeat(80));
    print_tree_structure(&tree, &source, max_depth);
    
    // Print AST
    println!("\nParsed AST:");
    println!("{}", "=".repeat(80));
    let (items, errors) = parser.parse_ast(&source);
    
    if !errors.is_empty() {
        println!("Parsing errors:");
        for (i, error) in errors.iter().enumerate() {
            println!("  {}. {}", i + 1, error);
        }
        println!();
    }
    
    println!("Items: {}", items.len());
    for (i, item) in items.iter().enumerate() {
        println!("Item {}: {:?}", i + 1, item.kind);
    }
}

// Function to print the tree structure
fn print_tree_structure(tree: &tree_sitter::Tree, source: &str, max_depth: usize) {
    print_tree_structure_recursive(&tree.root_node(), source, 0, max_depth);
}

fn print_tree_structure_recursive(node: &tree_sitter::Node, source: &str, depth: usize, max_depth: usize) {
    if depth >= max_depth {
        return;
    }

    let indent = " ".repeat(depth * 2);
    let text = if node.child_count() == 0 {
        match node.utf8_text(source.as_bytes()) {
            Ok(t) => format!(" \"{}\"", t),
            Err(_) => String::new(),
        }
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