# Rust-Analyzer Patterns

Rust-analyzer is the primary real-world user of Rowan and demonstrates many advanced patterns for building language servers and IDE tools. This guide covers the patterns and techniques used in rust-analyzer that you can apply to your own language tools.

## Overview

Rust-analyzer showcases several key patterns:

1. **Typed AST wrappers** over generic syntax trees
2. **Salsa-based incremental computation** for caching
3. **LSP integration** with real-time parsing
4. **Error recovery** and partial parsing
5. **Semantic analysis** on top of syntax trees
6. **Code generation** from syntax trees

## Typed AST Wrappers

### The AST Trait Pattern

Rust-analyzer wraps Rowan's generic `SyntaxNode` in typed AST nodes that provide domain-specific APIs:

```rust
use rowan::{SyntaxNode, SyntaxToken, Language};

// Base trait for all AST nodes
trait AstNode: Clone {
    type Language: Language;
    
    fn can_cast(kind: <Self::Language as Language>::Kind) -> bool;
    fn cast(syntax: SyntaxNode<Self::Language>) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode<Self::Language>;
}

// Example: Function declaration AST node
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    syntax: SyntaxNode<RustLanguage>,
}

impl AstNode for Function {
    type Language = RustLanguage;
    
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::FN
    }
    
    fn cast(syntax: SyntaxNode<RustLanguage>) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    
    fn syntax(&self) -> &SyntaxNode<RustLanguage> {
        &self.syntax
    }
}

impl Function {
    // Domain-specific methods
    pub fn name(&self) -> Option<Name> {
        self.syntax
            .children()
            .find_map(Name::cast)
    }
    
    pub fn param_list(&self) -> Option<ParamList> {
        self.syntax
            .children()
            .find_map(ParamList::cast)
    }
    
    pub fn body(&self) -> Option<BlockExpr> {
        self.syntax
            .children()
            .find_map(BlockExpr::cast)
    }
    
    pub fn ret_type(&self) -> Option<RetType> {
        self.syntax
            .children()
            .find_map(RetType::cast)
    }
    
    pub fn is_async(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|it| it.kind() == SyntaxKind::ASYNC_KW)
    }
    
    pub fn visibility(&self) -> Option<Visibility> {
        self.syntax
            .children()
            .find_map(Visibility::cast)
    }
}
```

### AST Token Wrappers

```rust
// Wrapper for identifier tokens
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    syntax: SyntaxNode<RustLanguage>,
}

impl Name {
    pub fn ident_token(&self) -> Option<SyntaxToken<RustLanguage>> {
        self.syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == SyntaxKind::IDENT)
    }
    
    pub fn text(&self) -> String {
        self.ident_token()
            .map(|it| it.text().to_string())
            .unwrap_or_default()
    }
}

// Wrapper for string literals
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct String_ {
    syntax: SyntaxNode<RustLanguage>,
}

impl String_ {
    pub fn value(&self) -> Option<String> {
        let token = self.syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == SyntaxKind::STRING)?;
        
        // Parse string literal, handling escapes
        parse_string_literal(token.text())
    }
    
    pub fn quote_offsets(&self) -> Option<(TextSize, TextSize)> {
        let token = self.syntax.first_token()?;
        let text = token.text();
        
        if text.starts_with('"') && text.ends_with('"') {
            Some((TextSize::from(1), TextSize::from(text.len() as u32 - 1)))
        } else {
            None
        }
    }
}
```

### Visitor Pattern for AST

```rust
// Visitor trait for traversing typed AST
trait Visitor<'ast> {
    fn visit_function(&mut self, func: &'ast Function) {
        walk_function(self, func);
    }
    
    fn visit_struct(&mut self, struct_: &'ast Struct) {
        walk_struct(self, struct_);
    }
    
    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr);
    }
    
    fn visit_name(&mut self, name: &'ast Name) {
        // Default: do nothing
    }
}

// Walking functions
fn walk_function<'ast, V: Visitor<'ast>>(visitor: &mut V, func: &'ast Function) {
    if let Some(name) = func.name() {
        visitor.visit_name(&name);
    }
    
    if let Some(param_list) = func.param_list() {
        for param in param_list.params() {
            visitor.visit_param(&param);
        }
    }
    
    if let Some(body) = func.body() {
        visitor.visit_expr(&Expr::BlockExpr(body));
    }
}

// Example visitor: collect all function names
struct FunctionNameCollector {
    names: Vec<String>,
}

impl<'ast> Visitor<'ast> for FunctionNameCollector {
    fn visit_function(&mut self, func: &'ast Function) {
        if let Some(name) = func.name() {
            self.names.push(name.text());
        }
        
        // Continue walking
        walk_function(self, func);
    }
}
```

## Salsa Integration

### Database-Driven Architecture

Rust-analyzer uses Salsa for incremental computation, caching expensive operations:

```rust
use salsa::{Database, Durability};

// Database trait defining all queries
#[salsa::query_group(SourceDatabaseStorage)]
trait SourceDatabase: Database {
    // Input: source text
    #[salsa::input]
    fn source_text(&self, file_id: FileId) -> Arc<String>;
    
    // Derived: parse tree
    fn parse(&self, file_id: FileId) -> Parse<SourceFile>;
    
    // Derived: semantic analysis
    fn analyze(&self, file_id: FileId) -> Arc<Analysis>;
}

// Implementation of derived queries
fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse<SourceFile> {
    let text = db.source_text(file_id);
    
    // This will be cached by Salsa
    parse_source_file(&text)
}

fn analyze(db: &dyn SourceDatabase, file_id: FileId) -> Arc<Analysis> {
    let parse = db.parse(file_id);
    
    // Semantic analysis on top of syntax tree
    Arc::new(analyze_syntax_tree(parse.tree()))
}

// Database implementation
#[salsa::database(SourceDatabaseStorage)]
struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {}
```

### Incremental Parsing with Salsa

```rust
// Parse query with incremental optimization
fn parse_with_incremental(db: &dyn SourceDatabase, file_id: FileId) -> Parse<SourceFile> {
    let text = db.source_text(file_id);
    
    // Try to get previous parse result
    if let Some(old_parse) = db.maybe_changed_after(file_id) {
        // Compute text diff
        let old_text = old_parse.source_text();
        let edit = compute_edit(&old_text, &text);
        
        if let Some(edit) = edit {
            // Incremental reparse
            return incremental_reparse(old_parse.tree(), edit, &text);
        }
    }
    
    // Full parse
    parse_source_file(&text)
}

// Invalidation handling
impl RootDatabase {
    pub fn apply_change(&mut self, change: Change) {
        for (file_id, new_text) in change.files {
            // This will invalidate all dependent queries
            self.set_source_text_with_durability(
                file_id, 
                Arc::new(new_text),
                Durability::LOW
            );
        }
    }
}
```

## LSP Integration Patterns

### Document Synchronization

```rust
use lsp_types::*;
use std::collections::HashMap;

struct LanguageServer {
    db: RootDatabase,
    file_map: HashMap<Url, FileId>,
    next_file_id: u32,
}

impl LanguageServer {
    fn handle_did_open(&mut self, params: DidOpenTextDocumentParams) {
        let file_id = FileId(self.next_file_id);
        self.next_file_id += 1;
        
        self.file_map.insert(params.text_document.uri.clone(), file_id);
        
        // Set initial content
        self.db.set_source_text(file_id, Arc::new(params.text_document.text));
        
        // Trigger initial analysis
        let _ = self.db.analyze(file_id);
    }
    
    fn handle_did_change(&mut self, params: DidChangeTextDocumentParams) {
        let file_id = self.file_map[&params.text_document.uri];
        
        // Apply incremental changes
        let mut text = (*self.db.source_text(file_id)).clone();
        
        for change in params.content_changes {
            if let Some(range) = change.range {
                // Incremental change
                let start = position_to_offset(&text, range.start);
                let end = position_to_offset(&text, range.end);
                text.replace_range(start..end, &change.text);
            } else {
                // Full document replacement
                text = change.text;
            }
        }
        
        // Update database
        self.db.set_source_text(file_id, Arc::new(text));
    }
}
```

### Real-time Diagnostics

```rust
impl LanguageServer {
    fn handle_diagnostics_request(&self, file_id: FileId) -> Vec<Diagnostic> {
        let parse = self.db.parse(file_id);
        let analysis = self.db.analyze(file_id);
        
        let mut diagnostics = Vec::new();
        
        // Syntax errors
        for error in parse.errors() {
            diagnostics.push(Diagnostic {
                range: text_range_to_lsp_range(error.range()),
                severity: Some(DiagnosticSeverity::ERROR),
                message: error.message().to_string(),
                source: Some("syntax".to_string()),
                ..Default::default()
            });
        }
        
        // Semantic errors
        for error in analysis.errors() {
            diagnostics.push(Diagnostic {
                range: text_range_to_lsp_range(error.range()),
                severity: Some(error.severity()),
                message: error.message().to_string(),
                source: Some("semantic".to_string()),
                ..Default::default()
            });
        }
        
        diagnostics
    }
    
    fn handle_completion(&self, params: CompletionParams) -> Vec<CompletionItem> {
        let file_id = self.file_map[&params.text_document_position.text_document.uri];
        let position = params.text_document_position.position;
        
        let parse = self.db.parse(file_id);
        let offset = lsp_position_to_offset(&parse.source_text(), position);
        
        // Find token at position
        let token = parse.tree().token_at_offset(offset);
        
        // Generate completions based on context
        self.completions_at_position(file_id, token)
    }
}
```

## Error Recovery Patterns

### Resilient Parsing

```rust
struct ResilientParser {
    tokens: Vec<Token>,
    pos: usize,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<SyntaxError>,
}

impl ResilientParser {
    fn parse_item(&mut self) {
        let checkpoint = self.builder.checkpoint();
        
        match self.current_kind() {
            Some(SyntaxKind::FN_KW) => {
                if self.try_parse_function() {
                    return;
                }
            }
            Some(SyntaxKind::STRUCT_KW) => {
                if self.try_parse_struct() {
                    return;
                }
            }
            _ => {}
        }
        
        // Error recovery: create error node and skip to next item
        self.builder.start_node_at(checkpoint, SyntaxKind::ERROR.into());
        self.error("expected item");
        self.skip_to_item_boundary();
        self.builder.finish_node();
    }
    
    fn try_parse_function(&mut self) -> bool {
        let checkpoint = self.builder.checkpoint();
        
        self.builder.start_node_at(checkpoint, SyntaxKind::FN.into());
        
        if !self.expect(SyntaxKind::FN_KW) {
            self.builder.finish_node();
            return false;
        }
        
        // Try to parse name
        if !self.expect(SyntaxKind::IDENT) {
            // Error recovery: continue parsing even without name
            self.error("expected function name");
        }
        
        // Try to parse parameter list
        if !self.try_parse_param_list() {
            // Error recovery: assume empty parameter list
            self.builder.start_node(SyntaxKind::PARAM_LIST.into());
            self.builder.finish_node();
        }
        
        // Try to parse body
        if !self.try_parse_block() {
            // Error recovery: assume empty body
            self.builder.start_node(SyntaxKind::BLOCK_EXPR.into());
            self.builder.finish_node();
        }
        
        self.builder.finish_node();
        true
    }
    
    fn skip_to_item_boundary(&mut self) {
        while !self.at_end() && !self.at_item_start() {
            self.advance();
        }
    }
    
    fn at_item_start(&self) -> bool {
        matches!(self.current_kind(), 
            Some(SyntaxKind::FN_KW) | 
            Some(SyntaxKind::STRUCT_KW) |
            Some(SyntaxKind::ENUM_KW))
    }
}
```

### Error Reporting

```rust
#[derive(Debug, Clone)]
pub struct SyntaxError {
    message: String,
    range: TextRange,
    kind: ErrorKind,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedToken { expected: Vec<SyntaxKind>, found: SyntaxKind },
    MissingToken { expected: SyntaxKind },
    InvalidSyntax { context: String },
}

impl SyntaxError {
    pub fn unexpected_token(
        expected: Vec<SyntaxKind>, 
        found: SyntaxKind, 
        range: TextRange
    ) -> Self {
        let expected_names: Vec<_> = expected.iter().map(|k| format!("{:?}", k)).collect();
        let message = format!(
            "expected {}, found {:?}",
            expected_names.join(" or "),
            found
        );
        
        Self {
            message,
            range,
            kind: ErrorKind::UnexpectedToken { expected, found },
        }
    }
    
    pub fn missing_token(expected: SyntaxKind, range: TextRange) -> Self {
        Self {
            message: format!("expected {:?}", expected),
            range,
            kind: ErrorKind::MissingToken { expected },
        }
    }
    
    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic {
            range: text_range_to_lsp_range(self.range),
            severity: Some(DiagnosticSeverity::ERROR),
            message: self.message.clone(),
            source: Some("parser".to_string()),
            ..Default::default()
        }
    }
}
```

## Semantic Analysis Patterns

### Symbol Resolution

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    symbols: HashMap<SymbolId, Symbol>,
    next_symbol_id: u32,
}

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
    symbols: HashMap<String, SymbolId>,
    kind: ScopeKind,
}

#[derive(Debug, Clone)]
pub enum ScopeKind {
    Module,
    Function,
    Block,
    Struct,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    name: String,
    kind: SymbolKind,
    definition: SyntaxNode<RustLanguage>,
    visibility: Visibility,
}

impl SymbolTable {
    pub fn analyze_file(&mut self, file: &SourceFile) {
        let mut analyzer = SymbolAnalyzer::new(self);
        analyzer.visit_source_file(file);
    }
}

struct SymbolAnalyzer<'a> {
    table: &'a mut SymbolTable,
    current_scope: ScopeId,
}

impl<'a> SymbolAnalyzer<'a> {
    fn visit_function(&mut self, func: &Function) {
        // Create symbol for function
        let symbol_id = self.table.create_symbol(
            func.name().map(|n| n.text()).unwrap_or_default(),
            SymbolKind::Function,
            func.syntax().clone(),
        );
        
        // Add to current scope
        self.table.add_to_scope(self.current_scope, symbol_id);
        
        // Create new scope for function body
        let func_scope = self.table.create_scope(ScopeKind::Function, Some(self.current_scope));
        let old_scope = self.current_scope;
        self.current_scope = func_scope;
        
        // Analyze parameters
        if let Some(param_list) = func.param_list() {
            for param in param_list.params() {
                self.visit_param(&param);
            }
        }
        
        // Analyze body
        if let Some(body) = func.body() {
            self.visit_block_expr(&body);
        }
        
        // Restore scope
        self.current_scope = old_scope;
    }
}
```

### Type Inference

```rust
#[derive(Debug, Clone)]
pub struct TypeInference {
    type_vars: HashMap<TypeVarId, Type>,
    constraints: Vec<TypeConstraint>,
    next_type_var: u32,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Integer,
    String,
    Bool,
    Function { params: Vec<Type>, ret: Box<Type> },
    Struct { name: String, fields: HashMap<String, Type> },
    TypeVar(TypeVarId),
}

#[derive(Debug, Clone)]
pub enum TypeConstraint {
    Equal(Type, Type),
    Subtype(Type, Type),
}

impl TypeInference {
    pub fn infer_function(&mut self, func: &Function) -> Type {
        // Create type variables for parameters
        let param_types: Vec<_> = func.param_list()
            .map(|params| {
                params.params()
                    .map(|_| Type::TypeVar(self.fresh_type_var()))
                    .collect()
            })
            .unwrap_or_default();
        
        // Create type variable for return type
        let ret_type = Type::TypeVar(self.fresh_type_var());
        
        // Infer body type
        if let Some(body) = func.body() {
            let body_type = self.infer_expr(&Expr::BlockExpr(body));
            
            // Add constraint: body type = return type
            self.add_constraint(TypeConstraint::Equal(body_type, ret_type.clone()));
        }
        
        Type::Function {
            params: param_types,
            ret: Box::new(ret_type),
        }
    }
    
    fn infer_expr(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::LiteralExpr(lit) => self.infer_literal(lit),
            Expr::BinExpr(bin) => self.infer_binary_expr(bin),
            Expr::CallExpr(call) => self.infer_call_expr(call),
            _ => Type::Unknown,
        }
    }
    
    fn solve_constraints(&mut self) {
        // Unification algorithm
        for constraint in &self.constraints.clone() {
            match constraint {
                TypeConstraint::Equal(t1, t2) => {
                    self.unify(t1.clone(), t2.clone());
                }
                TypeConstraint::Subtype(t1, t2) => {
                    // Simplified subtyping
                    self.unify(t1.clone(), t2.clone());
                }
            }
        }
    }
}
```

## Code Generation Patterns

### Syntax Tree Transformation

```rust
pub struct CodeGenerator {
    output: String,
    indent_level: usize,
}

impl CodeGenerator {
    pub fn generate(&mut self, file: &SourceFile) {
        for item in file.items() {
            self.generate_item(&item);
        }
    }
    
    fn generate_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => self.generate_function(func),
            Item::Struct(struct_) => self.generate_struct(struct_),
            Item::Enum(enum_) => self.generate_enum(enum_),
            _ => {}
        }
    }
    
    fn generate_function(&mut self, func: &Function) {
        // Generate function signature
        if let Some(vis) = func.visibility() {
            self.write(&format!("{} ", vis.syntax().text()));
        }
        
        if func.is_async() {
            self.write("async ");
        }
        
        self.write("fn ");
        
        if let Some(name) = func.name() {
            self.write(&name.text());
        }
        
        // Generate parameter list
        if let Some(param_list) = func.param_list() {
            self.write("(");
            for (i, param) in param_list.params().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.generate_param(&param);
            }
            self.write(")");
        }
        
        // Generate return type
        if let Some(ret_type) = func.ret_type() {
            self.write(" -> ");
            self.generate_type(&ret_type.ty().unwrap());
        }
        
        // Generate body
        if let Some(body) = func.body() {
            self.write(" ");
            self.generate_block_expr(&body);
        }
        
        self.writeln("");
    }
    
    fn write(&mut self, text: &str) {
        self.output.push_str(text);
    }
    
    fn writeln(&mut self, text: &str) {
        self.output.push_str(text);
        self.output.push('\n');
        
        // Add indentation for next line
        for _ in 0..self.indent_level {
            self.output.push_str("    ");
        }
    }
    
    fn indent(&mut self) {
        self.indent_level += 1;
    }
    
    fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }
}
```

### Macro Expansion

```rust
pub struct MacroExpander {
    expansions: HashMap<String, MacroDefinition>,
}

#[derive(Debug, Clone)]
pub struct MacroDefinition {
    name: String,
    pattern: MacroPattern,
    expansion: MacroExpansion,
}

impl MacroExpander {
    pub fn expand_macro_call(&self, call: &MacroCall) -> Option<SyntaxNode<RustLanguage>> {
        let name = call.path()?.segment()?.name_ref()?.text();
        let definition = self.expansions.get(&name)?;
        
        // Match macro arguments against pattern
        let bindings = self.match_pattern(&definition.pattern, call.token_tree()?)?;
        
        // Expand using bindings
        self.expand_with_bindings(&definition.expansion, &bindings)
    }
    
    fn match_pattern(
        &self, 
        pattern: &MacroPattern, 
        input: &TokenTree
    ) -> Option<HashMap<String, Vec<SyntaxToken<RustLanguage>>>> {
        // Simplified pattern matching
        let mut bindings = HashMap::new();
        
        // Match tokens against pattern
        for (pattern_token, input_token) in pattern.tokens.iter().zip(input.tokens()) {
            match pattern_token {
                PatternToken::Literal(text) => {
                    if input_token.text() != text {
                        return None;
                    }
                }
                PatternToken::Variable(name) => {
                    bindings.entry(name.clone()).or_insert_with(Vec::new).push(input_token);
                }
            }
        }
        
        Some(bindings)
    }
    
    fn expand_with_bindings(
        &self,
        expansion: &MacroExpansion,
        bindings: &HashMap<String, Vec<SyntaxToken<RustLanguage>>>
    ) -> Option<SyntaxNode<RustLanguage>> {
        let mut builder = GreenNodeBuilder::new();
        
        for token in &expansion.tokens {
            match token {
                ExpansionToken::Literal(text) => {
                    builder.token(SyntaxKind::IDENT.into(), text);
                }
                ExpansionToken::Variable(name) => {
                    if let Some(bound_tokens) = bindings.get(name) {
                        for bound_token in bound_tokens {
                            builder.token(bound_token.kind().into(), bound_token.text());
                        }
                    }
                }
            }
        }
        
        Some(SyntaxNode::new_root(builder.finish()))
    }
}
```

## Performance Patterns

### Lazy Computation

```rust
use std::cell::OnceCell;

pub struct LazyAnalysis {
    syntax: SyntaxNode<RustLanguage>,
    symbols: OnceCell<SymbolTable>,
    types: OnceCell<TypeInference>,
    diagnostics: OnceCell<Vec<Diagnostic>>,
}

impl LazyAnalysis {
    pub fn new(syntax: SyntaxNode<RustLanguage>) -> Self {
        Self {
            syntax,
            symbols: OnceCell::new(),
            types: OnceCell::new(),
            diagnostics: OnceCell::new(),
        }
    }
    
    pub fn symbols(&self) -> &SymbolTable {
        self.symbols.get_or_init(|| {
            let mut table = SymbolTable::new();
            if let Some(file) = SourceFile::cast(self.syntax.clone()) {
                table.analyze_file(&file);
            }
            table
        })
    }
    
    pub fn types(&self) -> &TypeInference {
        self.types.get_or_init(|| {
            let mut inference = TypeInference::new();
            if let Some(file) = SourceFile::cast(self.syntax.clone()) {
                inference.infer_file(&file);
            }
            inference
        })
    }
    
    pub fn diagnostics(&self) -> &[Diagnostic] {
        self.diagnostics.get_or_init(|| {
            let mut diagnostics = Vec::new();
            
            // Collect syntax errors
            // Collect semantic errors from symbols and types
            
            diagnostics
        })
    }
}
```

### Caching Strategies

```rust
use std::sync::Arc;

#[derive(Clone)]
pub struct CachedAnalysis {
    inner: Arc<LazyAnalysis>,
    cache_key: u64,
}

impl CachedAnalysis {
    pub fn new(syntax: SyntaxNode<RustLanguage>) -> Self {
        let cache_key = compute_syntax_hash(&syntax);
        
        Self {
            inner: Arc::new(LazyAnalysis::new(syntax)),
            cache_key,
        }
    }
    
    pub fn is_valid_for(&self, syntax: &SyntaxNode<RustLanguage>) -> bool {
        self.cache_key == compute_syntax_hash(syntax)
    }
}

fn compute_syntax_hash(syntax: &SyntaxNode<RustLanguage>) -> u64 {
    use std::hash::{Hash, Hasher};
    use std::collections::hash_map::DefaultHasher;
    
    let mut hasher = DefaultHasher::new();
    syntax.text().hash(&mut hasher);
    hasher.finish()
}
```

These patterns from rust-analyzer demonstrate how to build sophisticated language tools on top of Rowan's foundation. The key insights are:

1. **Layer typed APIs** over generic syntax trees
2. **Use incremental computation** for performance
3. **Implement robust error recovery** for real-world code
4. **Cache aggressively** but invalidate correctly
5. **Separate syntax from semantics** but keep them connected

By following these patterns, you can build language servers and IDE tools that are both powerful and performant.