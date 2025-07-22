# Tree Traversal and Manipulation

This guide covers how to navigate, query, and manipulate Rowan syntax trees. Rowan provides a rich API for tree traversal that balances performance with convenience.

## Overview

Rowan provides several categories of traversal operations:

- **Direct navigation**: Parent, children, siblings
- **Iterative traversal**: Descendants, ancestors, preorder walks
- **Positional queries**: Token at offset, range queries
- **Filtered searches**: Finding nodes by kind or predicate
- **Tree modification**: Creating new trees with changes

## Basic Navigation

### Parent and Children

```rust
use rowan::{SyntaxNode, Language};

fn explore_family_relationships<L: Language>(node: &SyntaxNode<L>) {
    // Get parent
    if let Some(parent) = node.parent() {
        println!("Parent: {:?}", parent.kind());
    }
    
    // Get children (nodes only)
    for child in node.children() {
        println!("Child node: {:?}", child.kind());
    }
    
    // Get children including tokens
    for element in node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(node) => {
                println!("Child node: {:?}", node.kind());
            }
            rowan::NodeOrToken::Token(token) => {
                println!("Child token: {:?} = {:?}", token.kind(), token.text());
            }
        }
    }
    
    // Get first/last children
    if let Some(first) = node.first_child() {
        println!("First child: {:?}", first.kind());
    }
    
    if let Some(last) = node.last_child() {
        println!("Last child: {:?}", last.kind());
    }
}
```

### Sibling Navigation

```rust
fn explore_siblings<L: Language>(node: &SyntaxNode<L>) {
    // Navigate to next/previous siblings
    let mut current = Some(node.clone());
    
    // Walk forward through siblings
    while let Some(sibling) = current {
        println!("Sibling: {:?}", sibling.kind());
        current = sibling.next_sibling();
    }
    
    // Walk backward through siblings
    let mut current = Some(node.clone());
    while let Some(sibling) = current {
        println!("Sibling: {:?}", sibling.kind());
        current = sibling.prev_sibling();
    }
    
    // Include tokens in sibling traversal
    for element in node.siblings_with_tokens(rowan::Direction::Next) {
        match element {
            rowan::NodeOrToken::Node(node) => println!("Sibling node: {:?}", node.kind()),
            rowan::NodeOrToken::Token(token) => println!("Sibling token: {:?}", token.kind()),
        }
    }
}
```

### Token Access

```rust
fn explore_tokens<L: Language>(node: &SyntaxNode<L>) {
    // Get first and last tokens in subtree
    if let Some(first_token) = node.first_token() {
        println!("First token: {:?} = {:?}", first_token.kind(), first_token.text());
    }
    
    if let Some(last_token) = node.last_token() {
        println!("Last token: {:?} = {:?}", last_token.kind(), last_token.text());
    }
    
    // Get text range and content
    println!("Node text: {:?}", node.text());
    println!("Node range: {:?}", node.text_range());
}
```

## Iterative Traversal

### Descendants

```rust
fn find_all_identifiers<L: Language>(root: &SyntaxNode<L>) -> Vec<String> {
    let mut identifiers = Vec::new();
    
    // Traverse all descendant nodes
    for node in root.descendants() {
        if node.kind() == SyntaxKind::IDENT {
            if let Some(token) = node.first_token() {
                identifiers.push(token.text().to_string());
            }
        }
    }
    
    // Alternative: traverse nodes and tokens
    for element in root.descendants_with_tokens() {
        if let rowan::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::IDENT {
                identifiers.push(token.text().to_string());
            }
        }
    }
    
    identifiers
}
```

### Ancestors

```rust
fn find_containing_function<L: Language>(node: &SyntaxNode<L>) -> Option<SyntaxNode<L>> {
    // Walk up the tree to find a function
    for ancestor in node.ancestors() {
        if ancestor.kind() == SyntaxKind::FUNCTION {
            return Some(ancestor);
        }
    }
    None
}

fn get_scope_chain<L: Language>(node: &SyntaxNode<L>) -> Vec<SyntaxNode<L>> {
    // Collect all scoping ancestors
    node.ancestors()
        .filter(|ancestor| matches!(ancestor.kind(), 
            SyntaxKind::FUNCTION | SyntaxKind::BLOCK | SyntaxKind::MODULE))
        .collect()
}
```

### Preorder Traversal

```rust
use rowan::WalkEvent;

fn preorder_walk<L: Language>(root: &SyntaxNode<L>) {
    // Walk with enter/leave events
    for event in root.preorder_with_tokens() {
        match event {
            WalkEvent::Enter(element) => {
                match element {
                    rowan::NodeOrToken::Node(node) => {
                        println!("Entering node: {:?}", node.kind());
                    }
                    rowan::NodeOrToken::Token(token) => {
                        println!("Token: {:?} = {:?}", token.kind(), token.text());
                    }
                }
            }
            WalkEvent::Leave(element) => {
                if let rowan::NodeOrToken::Node(node) = element {
                    println!("Leaving node: {:?}", node.kind());
                }
            }
        }
    }
}

fn calculate_depth<L: Language>(root: &SyntaxNode<L>) -> usize {
    let mut max_depth = 0;
    let mut current_depth = 0;
    
    for event in root.preorder() {
        match event {
            WalkEvent::Enter(_) => {
                current_depth += 1;
                max_depth = max_depth.max(current_depth);
            }
            WalkEvent::Leave(_) => {
                current_depth -= 1;
            }
        }
    }
    
    max_depth
}
```

## Filtered Searches

### Finding Nodes by Kind

```rust
fn find_by_kind<L: Language>(root: &SyntaxNode<L>, target_kind: L::Kind) -> Vec<SyntaxNode<L>> {
    root.descendants()
        .filter(|node| node.kind() == target_kind)
        .collect()
}

fn find_first_by_kind<L: Language>(root: &SyntaxNode<L>, target_kind: L::Kind) -> Option<SyntaxNode<L>> {
    root.descendants()
        .find(|node| node.kind() == target_kind)
}

// Using built-in filtered methods
fn find_first_child_by_kind<L: Language>(node: &SyntaxNode<L>, target_kind: L::Kind) -> Option<SyntaxNode<L>> {
    node.first_child_by_kind(&|kind| kind == target_kind)
}
```

### Complex Queries

```rust
fn find_function_calls<L: Language>(root: &SyntaxNode<L>) -> Vec<SyntaxNode<L>> {
    root.descendants()
        .filter(|node| {
            node.kind() == SyntaxKind::CALL_EXPR &&
            node.first_child()
                .map(|child| child.kind() == SyntaxKind::PATH_EXPR)
                .unwrap_or(false)
        })
        .collect()
}

fn find_unused_variables<L: Language>(root: &SyntaxNode<L>) -> Vec<String> {
    let mut declared = std::collections::HashSet::new();
    let mut used = std::collections::HashSet::new();
    
    // Find all variable declarations
    for node in root.descendants() {
        if node.kind() == SyntaxKind::LET_STMT {
            if let Some(ident) = node.descendants_with_tokens()
                .filter_map(|element| element.into_token())
                .find(|token| token.kind() == SyntaxKind::IDENT)
            {
                declared.insert(ident.text().to_string());
            }
        }
    }
    
    // Find all variable uses
    for node in root.descendants() {
        if node.kind() == SyntaxKind::PATH_EXPR {
            if let Some(ident) = node.first_token() {
                if ident.kind() == SyntaxKind::IDENT {
                    used.insert(ident.text().to_string());
                }
            }
        }
    }
    
    declared.difference(&used).cloned().collect()
}
```

## Positional Queries

### Token at Offset

```rust
use rowan::{TokenAtOffset, TextSize};

fn find_token_at_position<L: Language>(
    root: &SyntaxNode<L>, 
    offset: TextSize
) -> TokenAtOffset<rowan::SyntaxToken<L>> {
    root.token_at_offset(offset)
}

fn find_node_at_position<L: Language>(
    root: &SyntaxNode<L>, 
    offset: TextSize
) -> Option<SyntaxNode<L>> {
    match root.token_at_offset(offset) {
        TokenAtOffset::None => None,
        TokenAtOffset::Single(token) => token.parent(),
        TokenAtOffset::Between(left, right) => {
            // Choose the token that makes more sense contextually
            // For example, prefer the right token for completion
            right.parent()
        }
    }
}

fn get_word_at_position<L: Language>(
    root: &SyntaxNode<L>, 
    offset: TextSize
) -> Option<String> {
    match root.token_at_offset(offset) {
        TokenAtOffset::Single(token) if token.kind() == SyntaxKind::IDENT => {
            Some(token.text().to_string())
        }
        TokenAtOffset::Between(left, right) => {
            if left.kind() == SyntaxKind::IDENT {
                Some(left.text().to_string())
            } else if right.kind() == SyntaxKind::IDENT {
                Some(right.text().to_string())
            } else {
                None
            }
        }
        _ => None,
    }
}
```

### Range Queries

```rust
use rowan::TextRange;

fn find_nodes_in_range<L: Language>(
    root: &SyntaxNode<L>, 
    range: TextRange
) -> Vec<SyntaxNode<L>> {
    root.descendants()
        .filter(|node| {
            let node_range = node.text_range();
            // Node is completely within the range
            range.contains_range(node_range) ||
            // Node overlaps with the range
            node_range.intersect(range).is_some()
        })
        .collect()
}

fn find_smallest_covering_node<L: Language>(
    root: &SyntaxNode<L>, 
    range: TextRange
) -> Option<SyntaxNode<L>> {
    root.descendants()
        .filter(|node| node.text_range().contains_range(range))
        .min_by_key(|node| node.text_range().len())
}
```

## Tree Modification

### Creating Modified Trees

```rust
use rowan::{GreenNodeBuilder, GreenNode};

fn replace_node<L: Language>(
    root: &SyntaxNode<L>,
    target: &SyntaxNode<L>,
    replacement: GreenNode
) -> SyntaxNode<L> {
    let new_green = target.replace_with(replacement);
    SyntaxNode::new_root(new_green)
}

fn insert_after_node<L: Language>(
    root: &SyntaxNode<L>,
    target: &SyntaxNode<L>,
    new_content: GreenNode
) -> SyntaxNode<L> {
    let parent = target.parent().unwrap();
    let mut builder = GreenNodeBuilder::new();
    
    builder.start_node(parent.kind().into());
    
    for child in parent.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(node) => {
                builder.add_child(node.green().clone().into());
                
                // Insert new content after target
                if node == *target {
                    builder.add_child(new_content.clone().into());
                }
            }
            rowan::NodeOrToken::Token(token) => {
                builder.token(token.kind().into(), token.text());
            }
        }
    }
    
    builder.finish_node();
    let new_parent = builder.finish();
    
    // Replace the parent in the tree
    parent.replace_with(new_parent).into()
}
```

### Batch Modifications

```rust
struct TreeEditor<L: Language> {
    changes: Vec<(SyntaxNode<L>, GreenNode)>,
}

impl<L: Language> TreeEditor<L> {
    fn new() -> Self {
        Self { changes: Vec::new() }
    }
    
    fn replace(&mut self, target: SyntaxNode<L>, replacement: GreenNode) {
        self.changes.push((target, replacement));
    }
    
    fn apply(self, root: SyntaxNode<L>) -> SyntaxNode<L> {
        let mut current = root;
        
        // Apply changes from deepest to shallowest to avoid invalidating positions
        let mut changes = self.changes;
        changes.sort_by_key(|(node, _)| std::cmp::Reverse(node.text_range().start()));
        
        for (target, replacement) in changes {
            current = SyntaxNode::new_root(target.replace_with(replacement));
        }
        
        current
    }
}
```

## Advanced Traversal Patterns

### Visitor Pattern

```rust
trait Visitor<L: Language> {
    fn visit_node(&mut self, node: &SyntaxNode<L>) {
        // Default implementation visits children
        for child in node.children() {
            self.visit_node(&child);
        }
    }
    
    fn visit_token(&mut self, token: &rowan::SyntaxToken<L>) {
        // Default implementation does nothing
    }
}

struct IdentifierCollector {
    identifiers: Vec<String>,
}

impl<L: Language> Visitor<L> for IdentifierCollector {
    fn visit_token(&mut self, token: &rowan::SyntaxToken<L>) {
        if token.kind() == SyntaxKind::IDENT {
            self.identifiers.push(token.text().to_string());
        }
    }
}

fn collect_identifiers<L: Language>(root: &SyntaxNode<L>) -> Vec<String> {
    let mut collector = IdentifierCollector { identifiers: Vec::new() };
    
    for element in root.descendants_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(node) => collector.visit_node(&node),
            rowan::NodeOrToken::Token(token) => collector.visit_token(&token),
        }
    }
    
    collector.identifiers
}
```

### Cursor-Based Traversal

```rust
use rowan::cursor::SyntaxNode as CursorNode;

fn efficient_deep_traversal<L: Language>(root: &SyntaxNode<L>) {
    let mut cursor = root.raw.clone(); // Access internal cursor
    
    // Cursor-based traversal is more memory efficient for deep trees
    loop {
        // Process current node
        println!("Visiting: {:?}", cursor.kind());
        
        // Try to go to first child
        if cursor.goto_first_child() {
            continue;
        }
        
        // Try to go to next sibling
        while !cursor.goto_next_sibling() {
            // Go back to parent
            if !cursor.goto_parent() {
                return; // Reached root
            }
        }
    }
}
```

### Parallel Traversal

```rust
use rayon::prelude::*;

fn parallel_search<L: Language + Send + Sync>(
    root: &SyntaxNode<L>, 
    predicate: impl Fn(&SyntaxNode<L>) -> bool + Send + Sync
) -> Vec<SyntaxNode<L>> 
where 
    L::Kind: Send + Sync 
{
    // Collect top-level children first
    let children: Vec<_> = root.children().collect();
    
    // Search each subtree in parallel
    children
        .par_iter()
        .flat_map(|child| {
            child.descendants()
                .filter(&predicate)
                .collect::<Vec<_>>()
        })
        .collect()
}
```

## Performance Considerations

### Efficient Patterns

```rust
// Good: Use iterators instead of collecting
fn count_functions<L: Language>(root: &SyntaxNode<L>) -> usize {
    root.descendants()
        .filter(|node| node.kind() == SyntaxKind::FUNCTION)
        .count()
}

// Good: Early termination
fn has_async_function<L: Language>(root: &SyntaxNode<L>) -> bool {
    root.descendants()
        .any(|node| {
            node.kind() == SyntaxKind::FUNCTION &&
            node.children_with_tokens()
                .any(|element| {
                    element.as_token()
                        .map(|token| token.kind() == SyntaxKind::ASYNC_KW)
                        .unwrap_or(false)
                })
        })
}

// Good: Cache frequently accessed nodes
struct CachedAnalysis<L: Language> {
    functions: Vec<SyntaxNode<L>>,
    variables: Vec<SyntaxNode<L>>,
}

impl<L: Language> CachedAnalysis<L> {
    fn new(root: &SyntaxNode<L>) -> Self {
        let functions = root.descendants()
            .filter(|node| node.kind() == SyntaxKind::FUNCTION)
            .collect();
            
        let variables = root.descendants()
            .filter(|node| node.kind() == SyntaxKind::LET_STMT)
            .collect();
            
        Self { functions, variables }
    }
}
```

### Memory Efficiency

```rust
// Good: Use references when possible
fn analyze_without_collecting<L: Language>(root: &SyntaxNode<L>) {
    for node in root.descendants() {
        if node.kind() == SyntaxKind::FUNCTION {
            analyze_function(&node); // Pass by reference
        }
    }
}

// Avoid: Unnecessary collections
fn avoid_collecting<L: Language>(root: &SyntaxNode<L>) {
    // Bad: Collecting all nodes first
    let all_nodes: Vec<_> = root.descendants().collect();
    for node in all_nodes {
        // Process node
    }
    
    // Good: Process as you go
    for node in root.descendants() {
        // Process node
    }
}
```

## Common Patterns

### AST Node Wrappers

```rust
// Wrapper for typed access to specific node kinds
struct FunctionNode<L: Language>(SyntaxNode<L>);

impl<L: Language> FunctionNode<L> {
    fn cast(node: SyntaxNode<L>) -> Option<Self> {
        if node.kind() == SyntaxKind::FUNCTION {
            Some(Self(node))
        } else {
            None
        }
    }
    
    fn name(&self) -> Option<String> {
        self.0.children_with_tokens()
            .filter_map(|element| element.into_token())
            .find(|token| token.kind() == SyntaxKind::IDENT)
            .map(|token| token.text().to_string())
    }
    
    fn parameters(&self) -> impl Iterator<Item = SyntaxNode<L>> + '_ {
        self.0.children()
            .filter(|child| child.kind() == SyntaxKind::PARAM)
    }
    
    fn body(&self) -> Option<SyntaxNode<L>> {
        self.0.children()
            .find(|child| child.kind() == SyntaxKind::BLOCK)
    }
}
```

### Error Recovery

```rust
fn find_errors<L: Language>(root: &SyntaxNode<L>) -> Vec<SyntaxNode<L>> {
    root.descendants()
        .filter(|node| node.kind() == SyntaxKind::ERROR)
        .collect()
}

fn validate_tree<L: Language>(root: &SyntaxNode<L>) -> Vec<String> {
    let mut errors = Vec::new();
    
    for node in root.descendants() {
        match node.kind() {
            SyntaxKind::FUNCTION => {
                if FunctionNode::cast(node.clone()).unwrap().name().is_none() {
                    errors.push("Function missing name".to_string());
                }
            }
            SyntaxKind::ERROR => {
                errors.push(format!("Parse error at {:?}", node.text_range()));
            }
            _ => {}
        }
    }
    
    errors
}
```

Tree traversal in Rowan is designed to be both powerful and efficient. The key is choosing the right traversal method for your use case and avoiding unnecessary allocations when working with large trees.