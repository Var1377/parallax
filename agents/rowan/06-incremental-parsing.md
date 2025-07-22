# Incremental Parsing Strategies

Incremental parsing is one of Rowan's key strengths, enabling efficient re-parsing of modified source code by reusing unchanged portions of the syntax tree. This is essential for responsive language servers and IDEs.

## Overview

Incremental parsing in Rowan works by:

1. **Identifying unchanged regions** in the source text
2. **Reusing green nodes** from the previous parse for unchanged regions
3. **Re-parsing only modified regions** and their minimal containing scope
4. **Reconstructing the tree** with a mix of old and new nodes

The green tree's immutability and structural sharing make this approach both safe and efficient.

## Basic Incremental Parsing

### Text Edit Representation

```rust
use text_size::{TextRange, TextSize};

#[derive(Debug, Clone)]
pub struct TextEdit {
    pub delete: TextRange,
    pub insert: String,
}

impl TextEdit {
    pub fn new(delete: TextRange, insert: String) -> Self {
        Self { delete, insert }
    }
    
    pub fn insert(offset: TextSize, text: String) -> Self {
        Self {
            delete: TextRange::at(offset, TextSize::from(0)),
            insert: text,
        }
    }
    
    pub fn delete(range: TextRange) -> Self {
        Self {
            delete: range,
            insert: String::new(),
        }
    }
    
    pub fn replace(range: TextRange, text: String) -> Self {
        Self {
            delete: range,
            insert: text,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EditedText {
    pub old_text: String,
    pub new_text: String,
    pub edit: TextEdit,
}

impl EditedText {
    pub fn apply_edit(old_text: String, edit: TextEdit) -> Self {
        let mut new_text = old_text.clone();
        let start = edit.delete.start().into();
        let end = edit.delete.end().into();
        
        new_text.replace_range(start..end, &edit.insert);
        
        Self {
            old_text,
            new_text,
            edit,
        }
    }
}
```

### Incremental Parser Structure

```rust
use rowan::{GreenNode, GreenNodeBuilder, Language, SyntaxNode};
use std::collections::HashMap;

pub struct IncrementalParser<L: Language> {
    cache: ParseCache<L>,
}

struct ParseCache<L: Language> {
    // Cache of parsed subtrees by their text range in the original source
    nodes: HashMap<TextRange, GreenNode>,
    // The last complete parse result
    last_tree: Option<GreenNode>,
    // The source text of the last parse
    last_text: String,
}

impl<L: Language> IncrementalParser<L> {
    pub fn new() -> Self {
        Self {
            cache: ParseCache {
                nodes: HashMap::new(),
                last_tree: None,
                last_text: String::new(),
            },
        }
    }
    
    pub fn parse(&mut self, text: &str) -> GreenNode {
        if self.cache.last_tree.is_none() {
            // First parse - no incremental optimization possible
            return self.full_parse(text);
        }
        
        // Find the differences between old and new text
        let edit = self.compute_edit(&self.cache.last_text, text);
        
        if edit.is_none() {
            // No changes - return cached result
            return self.cache.last_tree.as_ref().unwrap().clone();
        }
        
        self.incremental_parse(text, edit.unwrap())
    }
    
    fn full_parse(&mut self, text: &str) -> GreenNode {
        let tokens = lex(text);
        let mut parser = Parser::new(&tokens);
        let result = parser.parse();
        
        // Cache the result
        self.cache.last_tree = Some(result.green.clone());
        self.cache.last_text = text.to_string();
        
        result.green
    }
}
```

### Edit Detection

```rust
impl<L: Language> IncrementalParser<L> {
    fn compute_edit(&self, old_text: &str, new_text: &str) -> Option<TextEdit> {
        if old_text == new_text {
            return None;
        }
        
        // Find the longest common prefix
        let common_prefix = old_text
            .chars()
            .zip(new_text.chars())
            .take_while(|(a, b)| a == b)
            .count();
        
        // Find the longest common suffix
        let old_suffix = &old_text[common_prefix..];
        let new_suffix = &new_text[common_prefix..];
        
        let common_suffix = old_suffix
            .chars()
            .rev()
            .zip(new_suffix.chars().rev())
            .take_while(|(a, b)| a == b)
            .count();
        
        let delete_start = TextSize::from(common_prefix as u32);
        let delete_end = TextSize::from((old_text.len() - common_suffix) as u32);
        let insert_text = new_text[common_prefix..new_text.len() - common_suffix].to_string();
        
        Some(TextEdit {
            delete: TextRange::new(delete_start, delete_end),
            insert: insert_text,
        })
    }
}
```

## Advanced Incremental Strategies

### Range-Based Incremental Parsing

```rust
impl<L: Language> IncrementalParser<L> {
    fn incremental_parse(&mut self, new_text: &str, edit: TextEdit) -> GreenNode {
        let old_tree = self.cache.last_tree.as_ref().unwrap();
        let old_syntax = SyntaxNode::<L>::new_root(old_tree.clone());
        
        // Find the minimal range that needs reparsing
        let reparse_range = self.compute_reparse_range(&old_syntax, &edit);
        
        // Extract reusable nodes before and after the changed region
        let (prefix_nodes, suffix_nodes) = self.extract_reusable_nodes(
            &old_syntax, 
            reparse_range
        );
        
        // Parse only the changed region
        let new_middle = self.parse_range(new_text, reparse_range);
        
        // Reconstruct the tree
        let new_tree = self.reconstruct_tree(prefix_nodes, new_middle, suffix_nodes);
        
        // Update cache
        self.cache.last_tree = Some(new_tree.clone());
        self.cache.last_text = new_text.to_string();
        
        new_tree
    }
    
    fn compute_reparse_range(&self, old_tree: &SyntaxNode<L>, edit: &TextEdit) -> TextRange {
        // Start with the edit range
        let mut range = edit.delete;
        
        // Expand to include complete syntax constructs
        range = self.expand_to_syntax_boundaries(old_tree, range);
        
        // Expand to include any constructs that might be affected by the change
        range = self.expand_for_dependencies(old_tree, range);
        
        range
    }
    
    fn expand_to_syntax_boundaries(
        &self, 
        tree: &SyntaxNode<L>, 
        range: TextRange
    ) -> TextRange {
        // Find nodes that overlap with the range
        let overlapping_nodes: Vec<_> = tree
            .descendants()
            .filter(|node| {
                let node_range = node.text_range();
                node_range.intersect(range).is_some()
            })
            .collect();
        
        if overlapping_nodes.is_empty() {
            return range;
        }
        
        // Find the minimal containing nodes
        let containing_nodes: Vec<_> = overlapping_nodes
            .iter()
            .filter(|node| {
                // Node is a statement, declaration, or other complete construct
                matches!(node.kind(), 
                    SyntaxKind::FUNCTION |
                    SyntaxKind::STRUCT |
                    SyntaxKind::IMPL |
                    SyntaxKind::STATEMENT |
                    SyntaxKind::BLOCK)
            })
            .collect();
        
        if let Some(first) = containing_nodes.first() {
            if let Some(last) = containing_nodes.last() {
                return TextRange::new(
                    first.text_range().start(),
                    last.text_range().end(),
                );
            }
        }
        
        // Fallback: expand to cover all overlapping nodes
        let start = overlapping_nodes
            .iter()
            .map(|node| node.text_range().start())
            .min()
            .unwrap_or(range.start());
            
        let end = overlapping_nodes
            .iter()
            .map(|node| node.text_range().end())
            .max()
            .unwrap_or(range.end());
        
        TextRange::new(start, end)
    }
}
```

### Dependency-Aware Incremental Parsing

```rust
impl<L: Language> IncrementalParser<L> {
    fn expand_for_dependencies(
        &self, 
        tree: &SyntaxNode<L>, 
        range: TextRange
    ) -> TextRange {
        let mut expanded_range = range;
        
        // Check for constructs that might affect parsing of other regions
        for node in tree.descendants() {
            if !node.text_range().intersect(range).is_some() {
                continue;
            }
            
            match node.kind() {
                // Macro definitions affect macro calls throughout the file
                SyntaxKind::MACRO_DEF => {
                    expanded_range = TextRange::new(
                        TextSize::from(0),
                        tree.text_range().end(),
                    );
                }
                // Import statements affect name resolution
                SyntaxKind::USE_ITEM => {
                    expanded_range = self.expand_for_imports(tree, expanded_range);
                }
                // Type definitions affect type checking
                SyntaxKind::STRUCT | SyntaxKind::ENUM | SyntaxKind::TYPE_ALIAS => {
                    expanded_range = self.expand_for_type_changes(tree, expanded_range, &node);
                }
                _ => {}
            }
        }
        
        expanded_range
    }
    
    fn expand_for_imports(&self, tree: &SyntaxNode<L>, range: TextRange) -> TextRange {
        // If imports changed, we might need to reparse more
        // For simplicity, reparse everything after the import
        let import_end = tree
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::USE_ITEM)
            .map(|node| node.text_range().end())
            .max()
            .unwrap_or(range.end());
        
        TextRange::new(range.start(), tree.text_range().end().max(import_end))
    }
}
```

### Node Reuse Strategy

```rust
impl<L: Language> IncrementalParser<L> {
    fn extract_reusable_nodes(
        &self,
        old_tree: &SyntaxNode<L>,
        reparse_range: TextRange,
    ) -> (Vec<GreenNode>, Vec<GreenNode>) {
        let mut prefix_nodes = Vec::new();
        let mut suffix_nodes = Vec::new();
        
        for child in old_tree.children() {
            let child_range = child.text_range();
            
            if child_range.end() <= reparse_range.start() {
                // Node is completely before the changed region
                prefix_nodes.push(child.green().clone().into_owned());
            } else if child_range.start() >= reparse_range.end() {
                // Node is completely after the changed region
                // Need to adjust its position
                let adjusted_node = self.adjust_node_positions(
                    child.green().clone().into_owned(),
                    reparse_range,
                );
                suffix_nodes.push(adjusted_node);
            }
            // Nodes that overlap with the reparse range are discarded
        }
        
        (prefix_nodes, suffix_nodes)
    }
    
    fn adjust_node_positions(&self, node: GreenNode, edit_range: TextRange) -> GreenNode {
        // In practice, green nodes don't store absolute positions,
        // so no adjustment is needed. This is a placeholder for
        // more complex position tracking if needed.
        node
    }
    
    fn reconstruct_tree(
        &self,
        prefix_nodes: Vec<GreenNode>,
        middle_node: GreenNode,
        suffix_nodes: Vec<GreenNode>,
    ) -> GreenNode {
        let mut builder = GreenNodeBuilder::new();
        
        builder.start_node(SyntaxKind::ROOT.into());
        
        // Add prefix nodes
        for node in prefix_nodes {
            builder.add_child(node.into());
        }
        
        // Add the reparsed middle
        builder.add_child(middle_node.into());
        
        // Add suffix nodes
        for node in suffix_nodes {
            builder.add_child(node.into());
        }
        
        builder.finish_node();
        builder.finish()
    }
}
```

## Caching Strategies

### Multi-Level Caching

```rust
struct LayeredParseCache<L: Language> {
    // Cache for complete files
    file_cache: HashMap<String, (String, GreenNode)>, // (content_hash, tree)
    
    // Cache for individual constructs
    construct_cache: HashMap<ConstructKey, GreenNode>,
    
    // Cache for expressions and small constructs
    expression_cache: HashMap<String, GreenNode>, // (text, tree)
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct ConstructKey {
    kind: SyntaxKind,
    content_hash: u64,
    dependencies_hash: u64,
}

impl<L: Language> LayeredParseCache<L> {
    fn get_cached_construct(
        &self, 
        text: &str, 
        kind: SyntaxKind,
        dependencies: &[String]
    ) -> Option<GreenNode> {
        let content_hash = self.hash_content(text);
        let dependencies_hash = self.hash_dependencies(dependencies);
        
        let key = ConstructKey {
            kind,
            content_hash,
            dependencies_hash,
        };
        
        self.construct_cache.get(&key).cloned()
    }
    
    fn cache_construct(
        &mut self,
        text: &str,
        kind: SyntaxKind,
        dependencies: &[String],
        tree: GreenNode,
    ) {
        let content_hash = self.hash_content(text);
        let dependencies_hash = self.hash_dependencies(dependencies);
        
        let key = ConstructKey {
            kind,
            content_hash,
            dependencies_hash,
        };
        
        self.construct_cache.insert(key, tree);
    }
}
```

### Smart Cache Invalidation

```rust
impl<L: Language> LayeredParseCache<L> {
    fn invalidate_affected_caches(&mut self, edit: &TextEdit, old_tree: &SyntaxNode<L>) {
        // Find all constructs that might be affected by the edit
        let affected_constructs = self.find_affected_constructs(edit, old_tree);
        
        // Remove them from cache
        for construct in affected_constructs {
            self.construct_cache.remove(&construct);
        }
        
        // Invalidate expression cache entries that might be affected
        self.expression_cache.retain(|text, _| {
            !self.text_might_be_affected(text, edit)
        });
    }
    
    fn find_affected_constructs(
        &self, 
        edit: &TextEdit, 
        old_tree: &SyntaxNode<L>
    ) -> Vec<ConstructKey> {
        let mut affected = Vec::new();
        
        // Find nodes that overlap with the edit
        for node in old_tree.descendants() {
            if node.text_range().intersect(edit.delete).is_some() {
                if let Some(key) = self.node_to_cache_key(&node) {
                    affected.push(key);
                }
            }
        }
        
        affected
    }
}
```

## Parallel Incremental Parsing

### Concurrent Parsing Strategy

```rust
use rayon::prelude::*;
use std::sync::{Arc, Mutex};

struct ParallelIncrementalParser<L: Language> {
    cache: Arc<Mutex<ParseCache<L>>>,
    thread_pool: rayon::ThreadPool,
}

impl<L: Language + Send + Sync> ParallelIncrementalParser<L> 
where 
    L::Kind: Send + Sync 
{
    fn parse_parallel(&self, text: &str) -> GreenNode {
        let cache = self.cache.lock().unwrap();
        
        if let Some(old_tree) = &cache.last_tree {
            let edit = self.compute_edit(&cache.last_text, text);
            
            if let Some(edit) = edit {
                drop(cache); // Release lock before parallel work
                return self.incremental_parse_parallel(text, edit);
            }
        }
        
        drop(cache);
        self.full_parse_parallel(text)
    }
    
    fn incremental_parse_parallel(&self, text: &str, edit: TextEdit) -> GreenNode {
        let cache = self.cache.lock().unwrap();
        let old_tree = cache.last_tree.as_ref().unwrap().clone();
        drop(cache);
        
        let old_syntax = SyntaxNode::<L>::new_root(old_tree);
        
        // Identify independent regions that can be parsed in parallel
        let regions = self.identify_parallel_regions(&old_syntax, &edit);
        
        // Parse regions in parallel
        let parsed_regions: Vec<_> = regions
            .par_iter()
            .map(|region| {
                let region_text = &text[region.range.start().into()..region.range.end().into()];
                (region.clone(), self.parse_region(region_text, region.kind))
            })
            .collect();
        
        // Combine results
        self.combine_parallel_results(parsed_regions)
    }
    
    fn identify_parallel_regions(
        &self, 
        old_tree: &SyntaxNode<L>, 
        edit: &TextEdit
    ) -> Vec<ParseRegion> {
        let mut regions = Vec::new();
        
        // Find top-level constructs that can be parsed independently
        for child in old_tree.children() {
            let child_range = child.text_range();
            
            if child_range.intersect(edit.delete).is_some() {
                // This region needs reparsing
                regions.push(ParseRegion {
                    range: child_range,
                    kind: child.kind(),
                    dependencies: self.extract_dependencies(&child),
                });
            } else {
                // This region can be reused
                regions.push(ParseRegion {
                    range: child_range,
                    kind: child.kind(),
                    dependencies: Vec::new(), // No dependencies for reused nodes
                });
            }
        }
        
        regions
    }
}

#[derive(Debug, Clone)]
struct ParseRegion {
    range: TextRange,
    kind: SyntaxKind,
    dependencies: Vec<String>,
}
```

## Error Recovery in Incremental Parsing

### Graceful Degradation

```rust
impl<L: Language> IncrementalParser<L> {
    fn incremental_parse_with_fallback(
        &mut self, 
        text: &str, 
        edit: TextEdit
    ) -> GreenNode {
        // Try incremental parsing first
        match self.try_incremental_parse(text, &edit) {
            Ok(tree) => tree,
            Err(IncrementalParseError::InvalidEdit) => {
                // Edit is too complex for incremental parsing
                self.full_parse(text)
            }
            Err(IncrementalParseError::CacheCorruption) => {
                // Cache is corrupted, clear it and do full parse
                self.cache.clear();
                self.full_parse(text)
            }
            Err(IncrementalParseError::ParseError(e)) => {
                // Parse error in incremental region, try expanding the region
                let expanded_edit = self.expand_edit_range(&edit);
                match self.try_incremental_parse(text, &expanded_edit) {
                    Ok(tree) => tree,
                    Err(_) => {
                        // Still failing, fall back to full parse
                        self.full_parse(text)
                    }
                }
            }
        }
    }
    
    fn try_incremental_parse(
        &mut self, 
        text: &str, 
        edit: &TextEdit
    ) -> Result<GreenNode, IncrementalParseError> {
        // Validate that incremental parsing is possible
        self.validate_incremental_preconditions(text, edit)?;
        
        // Perform incremental parse
        let result = self.incremental_parse(text, edit.clone());
        
        // Validate the result
        self.validate_incremental_result(&result, text)?;
        
        Ok(result)
    }
    
    fn validate_incremental_preconditions(
        &self, 
        text: &str, 
        edit: &TextEdit
    ) -> Result<(), IncrementalParseError> {
        // Check if edit is within bounds
        if edit.delete.end() > TextSize::from(self.cache.last_text.len() as u32) {
            return Err(IncrementalParseError::InvalidEdit);
        }
        
        // Check if the edit is too large relative to the file
        let edit_ratio = edit.delete.len().into(): f64 / self.cache.last_text.len() as f64;
        if edit_ratio > 0.5 {
            return Err(IncrementalParseError::InvalidEdit);
        }
        
        Ok(())
    }
}

#[derive(Debug)]
enum IncrementalParseError {
    InvalidEdit,
    CacheCorruption,
    ParseError(String),
}
```

## Performance Optimization

### Metrics and Monitoring

```rust
#[derive(Debug, Default)]
struct IncrementalParseMetrics {
    total_parses: usize,
    incremental_parses: usize,
    full_parses: usize,
    cache_hits: usize,
    cache_misses: usize,
    average_reparse_ratio: f64,
}

impl<L: Language> IncrementalParser<L> {
    fn parse_with_metrics(&mut self, text: &str) -> (GreenNode, IncrementalParseMetrics) {
        let start_time = std::time::Instant::now();
        let mut metrics = IncrementalParseMetrics::default();
        
        metrics.total_parses += 1;
        
        let result = if self.cache.last_tree.is_none() {
            metrics.full_parses += 1;
            self.full_parse(text)
        } else {
            let edit = self.compute_edit(&self.cache.last_text, text);
            
            if let Some(edit) = edit {
                metrics.incremental_parses += 1;
                
                let reparse_ratio = edit.delete.len().into(): f64 / text.len() as f64;
                metrics.average_reparse_ratio = 
                    (metrics.average_reparse_ratio * (metrics.incremental_parses - 1) as f64 + reparse_ratio) 
                    / metrics.incremental_parses as f64;
                
                self.incremental_parse(text, edit)
            } else {
                metrics.cache_hits += 1;
                self.cache.last_tree.as_ref().unwrap().clone()
            }
        };
        
        let parse_time = start_time.elapsed();
        
        log::debug!(
            "Parse completed in {:?}: {} total, {} incremental, {} full, {:.2}% reparse ratio",
            parse_time,
            metrics.total_parses,
            metrics.incremental_parses,
            metrics.full_parses,
            metrics.average_reparse_ratio * 100.0
        );
        
        (result, metrics)
    }
}
```

### Adaptive Strategies

```rust
impl<L: Language> IncrementalParser<L> {
    fn should_use_incremental(&self, edit: &TextEdit) -> bool {
        // Don't use incremental for very small files
        if self.cache.last_text.len() < 1000 {
            return false;
        }
        
        // Don't use incremental for very large edits
        let edit_ratio = edit.delete.len().into(): f64 / self.cache.last_text.len() as f64;
        if edit_ratio > 0.3 {
            return false;
        }
        
        // Don't use incremental if we've had many cache misses recently
        if self.cache.recent_miss_rate() > 0.8 {
            return false;
        }
        
        true
    }
    
    fn adaptive_parse(&mut self, text: &str) -> GreenNode {
        if let Some(edit) = self.compute_edit(&self.cache.last_text, text) {
            if self.should_use_incremental(&edit) {
                return self.incremental_parse(text, edit);
            }
        }
        
        self.full_parse(text)
    }
}
```

## Integration with Language Servers

### LSP Integration

```rust
use lsp_types::{TextDocumentContentChangeEvent, VersionedTextDocumentIdentifier};

struct LanguageServer<L: Language> {
    parser: IncrementalParser<L>,
    document_versions: HashMap<String, i32>,
}

impl<L: Language> LanguageServer<L> {
    fn handle_document_change(
        &mut self,
        document: VersionedTextDocumentIdentifier,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<GreenNode, String> {
        let uri = document.uri.to_string();
        
        // Verify version ordering
        if let Some(&last_version) = self.document_versions.get(&uri) {
            if document.version <= last_version {
                return Err("Outdated document version".to_string());
            }
        }
        
        // Apply changes incrementally
        let mut current_text = self.get_document_text(&uri)?;
        
        for change in changes {
            if let Some(range) = change.range {
                // Incremental change
                let edit = TextEdit {
                    delete: lsp_range_to_text_range(range, &current_text),
                    insert: change.text,
                };
                
                current_text = apply_edit(current_text, edit);
            } else {
                // Full document replacement
                current_text = change.text;
            }
        }
        
        // Parse with incremental optimization
        let tree = self.parser.parse(&current_text);
        
        // Update version tracking
        self.document_versions.insert(uri, document.version);
        
        Ok(tree)
    }
}
```

Incremental parsing is crucial for responsive language tooling. The key is to balance the complexity of incremental strategies with their performance benefits, always having a fallback to full parsing when incremental parsing becomes too complex or error-prone.