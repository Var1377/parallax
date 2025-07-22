# Integration Patterns and Compiler-Specific Optimizations

## Table of Contents

1. [Introduction](#introduction)
2. [Compiler Integration Strategies](#compiler-integration-strategies)
3. [Memory Management Integration](#memory-management-integration)
4. [Performance-Critical Compiler Phases](#performance-critical-compiler-phases)
5. [LLVM Integration Patterns](#llvm-integration-patterns)
6. [Parallel Compilation Techniques](#parallel-compilation-techniques)
7. [Incremental Compilation Support](#incremental-compilation-support)
8. [Target-Specific Optimizations](#target-specific-optimizations)
9. [Runtime Integration](#runtime-integration)
10. [Best Practices](#best-practices)

## Introduction

This guide covers integration patterns for using bumpalo, smallvec, and indexmap in compiler implementations, with specific focus on the Parallax compiler architecture. These patterns demonstrate how to effectively combine these libraries to create high-performance compiler infrastructure.

### Parallax Compiler Architecture Overview

```rust
use bumpalo::Bump;
use smallvec::{SmallVec, smallvec};
use indexmap::IndexMap;

// Core compiler pipeline with optimized data structures
pub struct ParallaxCompiler {
    // Arena for temporary allocations during compilation
    arena: Bump,
    
    // Symbol table with insertion order preservation
    symbols: IndexMap<String, Symbol>,
    
    // Small collections for common compiler data
    current_scope: SmallVec<[ScopeId; 4]>,
    error_stack: SmallVec<[CompileError; 8]>,
    
    // Compilation phases
    lexer: Lexer,
    parser: Parser,
    type_checker: TypeChecker,
    code_generator: CodeGenerator,
}

impl ParallaxCompiler {
    pub fn compile(&mut self, source: &str) -> Result<CompiledModule, CompileError> {
        // Reset arena for new compilation unit
        self.arena.reset();
        
        // Tokenization phase
        let tokens = self.lexer.tokenize_in(&self.arena, source)?;
        
        // Parsing phase
        let ast = self.parser.parse_in(&self.arena, tokens)?;
        
        // Type checking phase
        let typed_ast = self.type_checker.check_in(&self.arena, ast)?;
        
        // Code generation phase
        let code = self.code_generator.generate(typed_ast)?;
        
        Ok(CompiledModule { code })
    }
}
```

## Compiler Integration Strategies

### Phase-Based Arena Management

```rust
use bumpalo::Bump;

// Separate arenas for different compilation phases
pub struct PhaseArenas {
    lexer_arena: Bump,
    parser_arena: Bump,
    type_checker_arena: Bump,
    optimizer_arena: Bump,
}

impl PhaseArenas {
    pub fn new() -> Self {
        Self {
            lexer_arena: Bump::new(),
            parser_arena: Bump::new(),
            type_checker_arena: Bump::new(),
            optimizer_arena: Bump::new(),
        }
    }
    
    pub fn reset_phase(&mut self, phase: CompilerPhase) {
        match phase {
            CompilerPhase::Lexing => self.lexer_arena.reset(),
            CompilerPhase::Parsing => self.parser_arena.reset(),
            CompilerPhase::TypeChecking => self.type_checker_arena.reset(),
            CompilerPhase::Optimization => self.optimizer_arena.reset(),
        }
    }
    
    pub fn get_arena(&self, phase: CompilerPhase) -> &Bump {
        match phase {
            CompilerPhase::Lexing => &self.lexer_arena,
            CompilerPhase::Parsing => &self.parser_arena,
            CompilerPhase::TypeChecking => &self.type_checker_arena,
            CompilerPhase::Optimization => &self.optimizer_arena,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompilerPhase {
    Lexing,
    Parsing,
    TypeChecking,
    Optimization,
}

// Usage in compiler pipeline
pub struct PipelineCompiler {
    arenas: PhaseArenas,
    current_phase: CompilerPhase,
}

impl PipelineCompiler {
    pub fn compile_module(&mut self, source: &str) -> Result<CompiledModule, CompileError> {
        // Lexing phase
        self.current_phase = CompilerPhase::Lexing;
        let arena = self.arenas.get_arena(self.current_phase);
        let tokens = self.tokenize_in(arena, source)?;
        
        // Parsing phase
        self.current_phase = CompilerPhase::Parsing;
        let arena = self.arenas.get_arena(self.current_phase);
        let ast = self.parse_in(arena, tokens)?;
        
        // Type checking phase
        self.current_phase = CompilerPhase::TypeChecking;
        let arena = self.arenas.get_arena(self.current_phase);
        let typed_ast = self.type_check_in(arena, ast)?;
        
        // Code generation (no arena needed - produces owned result)
        let code = self.generate_code(typed_ast)?;
        
        // Reset arenas for next compilation
        self.arenas.reset_phase(CompilerPhase::Lexing);
        self.arenas.reset_phase(CompilerPhase::Parsing);
        self.arenas.reset_phase(CompilerPhase::TypeChecking);
        
        Ok(CompiledModule { code })
    }
}
```

### Hierarchical Symbol Tables

```rust
use indexmap::IndexMap;
use smallvec::{SmallVec, smallvec};

// Symbol table with scope hierarchy
pub struct SymbolTable {
    // Global symbols with insertion order
    global_symbols: IndexMap<String, Symbol>,
    
    // Scope stack for nested scopes
    scope_stack: SmallVec<[ScopeId; 8]>,
    
    // Per-scope symbol tables
    scopes: IndexMap<ScopeId, IndexMap<String, Symbol>>,
    
    // Next scope ID
    next_scope_id: ScopeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            global_symbols: IndexMap::new(),
            scope_stack: smallvec![ScopeId(0)], // Global scope
            scopes: IndexMap::new(),
            next_scope_id: ScopeId(1),
        }
    }
    
    pub fn enter_scope(&mut self) -> ScopeId {
        let scope_id = self.next_scope_id;
        self.next_scope_id.0 += 1;
        
        self.scope_stack.push(scope_id);
        self.scopes.insert(scope_id, IndexMap::new());
        
        scope_id
    }
    
    pub fn exit_scope(&mut self) -> Option<ScopeId> {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop()
        } else {
            None // Can't exit global scope
        }
    }
    
    pub fn declare_symbol(&mut self, name: String, symbol: Symbol) -> Result<(), String> {
        let current_scope = *self.scope_stack.last().unwrap();
        
        if current_scope == ScopeId(0) {
            // Global scope
            if self.global_symbols.contains_key(&name) {
                return Err(format!("Symbol '{}' already declared in global scope", name));
            }
            self.global_symbols.insert(name, symbol);
        } else {
            // Local scope
            let scope_symbols = self.scopes.get_mut(&current_scope).unwrap();
            if scope_symbols.contains_key(&name) {
                return Err(format!("Symbol '{}' already declared in current scope", name));
            }
            scope_symbols.insert(name, symbol);
        }
        
        Ok(())
    }
    
    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        // Search from innermost to outermost scope
        for &scope_id in self.scope_stack.iter().rev() {
            if scope_id == ScopeId(0) {
                // Global scope
                if let Some(symbol) = self.global_symbols.get(name) {
                    return Some(symbol);
                }
            } else {
                // Local scope
                if let Some(scope_symbols) = self.scopes.get(&scope_id) {
                    if let Some(symbol) = scope_symbols.get(name) {
                        return Some(symbol);
                    }
                }
            }
        }
        
        None
    }
    
    pub fn symbols_in_declaration_order(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        // Return symbols in declaration order for diagnostics
        self.global_symbols.iter().chain(
            self.scopes.values().flat_map(|scope| scope.iter())
        )
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: SymbolType,
    pub location: SourceLocation,
    pub scope_id: ScopeId,
}

#[derive(Debug, Clone)]
pub enum SymbolType {
    Variable(TypeId),
    Function(FunctionSignature),
    Type(TypeDefinition),
    Module(ModuleId),
}
```

## Memory Management Integration

### Arena-Allocated AST Nodes

```rust
use bumpalo::Bump;
use smallvec::{SmallVec, smallvec};

// AST nodes allocated in arena
#[derive(Debug)]
pub struct AstNode<'arena> {
    pub kind: NodeKind<'arena>,
    pub span: SourceSpan,
    pub type_info: Option<TypeId>,
}

#[derive(Debug)]
pub enum NodeKind<'arena> {
    Literal(LiteralValue),
    Identifier(&'arena str),
    BinaryOp {
        op: BinaryOperator,
        left: &'arena AstNode<'arena>,
        right: &'arena AstNode<'arena>,
    },
    FunctionCall {
        function: &'arena AstNode<'arena>,
        args: &'arena [&'arena AstNode<'arena>],
    },
    Block {
        statements: &'arena [&'arena AstNode<'arena>],
    },
    // Small collections for common cases
    ParameterList {
        params: SmallVec<[Parameter<'arena>; 4]>,
    },
}

#[derive(Debug)]
pub struct Parameter<'arena> {
    pub name: &'arena str,
    pub type_annotation: Option<&'arena AstNode<'arena>>,
}

// Parser that allocates in arena
pub struct ArenaParser<'arena> {
    arena: &'arena Bump,
    tokens: &'arena [Token],
    current: usize,
}

impl<'arena> ArenaParser<'arena> {
    pub fn new(arena: &'arena Bump, tokens: &'arena [Token]) -> Self {
        Self {
            arena,
            tokens,
            current: 0,
        }
    }
    
    pub fn parse_expression(&mut self) -> Result<&'arena AstNode<'arena>, ParseError> {
        match self.current_token() {
            Token::Number(value) => {
                self.advance();
                Ok(self.arena.alloc(AstNode {
                    kind: NodeKind::Literal(LiteralValue::Number(*value)),
                    span: self.current_span(),
                    type_info: None,
                }))
            }
            Token::Identifier(name) => {
                self.advance();
                let name_str = self.arena.alloc_str(name);
                Ok(self.arena.alloc(AstNode {
                    kind: NodeKind::Identifier(name_str),
                    span: self.current_span(),
                    type_info: None,
                }))
            }
            Token::LeftParen => {
                self.parse_function_call()
            }
            _ => Err(ParseError::UnexpectedToken),
        }
    }
    
    fn parse_function_call(&mut self) -> Result<&'arena AstNode<'arena>, ParseError> {
        self.expect(Token::LeftParen)?;
        
        let function = self.parse_expression()?;
        
        // Use SmallVec for arguments (most functions have few args)
        let mut args = SmallVec::<[&'arena AstNode<'arena>; 4]>::new();
        
        while !self.check(Token::RightParen) {
            args.push(self.parse_expression()?);
            if !self.check(Token::RightParen) {
                self.expect(Token::Comma)?;
            }
        }
        
        self.expect(Token::RightParen)?;
        
        // Allocate args slice in arena
        let args_slice = self.arena.alloc_slice_fill_iter(args.iter().copied());
        
        Ok(self.arena.alloc(AstNode {
            kind: NodeKind::FunctionCall {
                function,
                args: args_slice,
            },
            span: self.current_span(),
            type_info: None,
        }))
    }
}
```

### Memory Pool for Frequent Allocations

```rust
use std::cell::RefCell;
use std::collections::VecDeque;

// Memory pool for frequently allocated objects
pub struct ObjectPool<T> {
    pool: RefCell<VecDeque<T>>,
    factory: Box<dyn Fn() -> T>,
}

impl<T> ObjectPool<T> {
    pub fn new<F>(factory: F) -> Self 
    where 
        F: Fn() -> T + 'static,
    {
        Self {
            pool: RefCell::new(VecDeque::new()),
            factory: Box::new(factory),
        }
    }
    
    pub fn get(&self) -> PooledObject<T> {
        let object = self.pool.borrow_mut().pop_front()
            .unwrap_or_else(|| (self.factory)());
        
        PooledObject {
            object: Some(object),
            pool: self,
        }
    }
    
    fn return_object(&self, object: T) {
        let mut pool = self.pool.borrow_mut();
        if pool.len() < 100 { // Limit pool size
            pool.push_back(object);
        }
    }
}

pub struct PooledObject<'a, T> {
    object: Option<T>,
    pool: &'a ObjectPool<T>,
}

impl<T> std::ops::Deref for PooledObject<'_, T> {
    type Target = T;
    
    fn deref(&self) -> &Self::Target {
        self.object.as_ref().unwrap()
    }
}

impl<T> std::ops::DerefMut for PooledObject<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.object.as_mut().unwrap()
    }
}

impl<T> Drop for PooledObject<'_, T> {
    fn drop(&mut self) {
        if let Some(object) = self.object.take() {
            self.pool.return_object(object);
        }
    }
}

// Usage in compiler
pub struct CompilerContext {
    string_pool: ObjectPool<String>,
    vec_pool: ObjectPool<Vec<Token>>,
    error_pool: ObjectPool<Vec<CompileError>>,
}

impl CompilerContext {
    pub fn new() -> Self {
        Self {
            string_pool: ObjectPool::new(|| String::with_capacity(64)),
            vec_pool: ObjectPool::new(|| Vec::with_capacity(32)),
            error_pool: ObjectPool::new(|| Vec::with_capacity(8)),
        }
    }
    
    pub fn get_string_buffer(&self) -> PooledObject<String> {
        let mut buffer = self.string_pool.get();
        buffer.clear();
        buffer
    }
    
    pub fn get_token_buffer(&self) -> PooledObject<Vec<Token>> {
        let mut buffer = self.vec_pool.get();
        buffer.clear();
        buffer
    }
}
```

## Performance-Critical Compiler Phases

### Optimized Lexer Implementation

```rust
use bumpalo::Bump;
use smallvec::{SmallVec, smallvec};

pub struct FastLexer<'arena> {
    arena: &'arena Bump,
    input: &'arena str,
    position: usize,
    line: u32,
    column: u32,
    
    // Small buffer for identifier/number parsing
    buffer: SmallVec<[u8; 32]>,
}

impl<'arena> FastLexer<'arena> {
    pub fn new(arena: &'arena Bump, input: &'arena str) -> Self {
        Self {
            arena,
            input,
            position: 0,
            line: 1,
            column: 1,
            buffer: SmallVec::new(),
        }
    }
    
    pub fn tokenize(&mut self) -> Result<&'arena [Token<'arena>], LexError> {
        let mut tokens = SmallVec::<[Token<'arena>; 256]>::new();
        
        while !self.is_at_end() {
            self.skip_whitespace();
            
            if self.is_at_end() {
                break;
            }
            
            let start_pos = self.position;
            let start_line = self.line;
            let start_column = self.column;
            
            let token = self.next_token()?;
            
            tokens.push(Token {
                kind: token,
                span: SourceSpan {
                    start: start_pos,
                    end: self.position,
                    line: start_line,
                    column: start_column,
                },
            });
        }
        
        // Allocate final token array in arena
        Ok(self.arena.alloc_slice_fill_iter(tokens.into_iter()))
    }
    
    fn next_token(&mut self) -> Result<TokenKind<'arena>, LexError> {
        let ch = self.advance();
        
        match ch {
            '+' => Ok(TokenKind::Plus),
            '-' => Ok(TokenKind::Minus),
            '*' => Ok(TokenKind::Star),
            '/' => Ok(TokenKind::Slash),
            '(' => Ok(TokenKind::LeftParen),
            ')' => Ok(TokenKind::RightParen),
            '{' => Ok(TokenKind::LeftBrace),
            '}' => Ok(TokenKind::RightBrace),
            '=' => {
                if self.match_char('=') {
                    Ok(TokenKind::EqualEqual)
                } else {
                    Ok(TokenKind::Equal)
                }
            }
            '"' => self.string_literal(),
            c if c.is_ascii_digit() => self.number_literal(),
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier_or_keyword(),
            _ => Err(LexError::UnexpectedCharacter(ch)),
        }
    }
    
    fn identifier_or_keyword(&mut self) -> Result<TokenKind<'arena>, LexError> {
        self.buffer.clear();
        
        // Back up to include the first character
        self.position -= 1;
        self.column -= 1;
        
        while !self.is_at_end() {
            let ch = self.peek();
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.buffer.push(self.advance() as u8);
            } else {
                break;
            }
        }
        
        let text = std::str::from_utf8(&self.buffer).unwrap();
        
        // Check for keywords
        let token_kind = match text {
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            "return" => TokenKind::Return,
            _ => {
                // Allocate identifier string in arena
                let identifier = self.arena.alloc_str(text);
                TokenKind::Identifier(identifier)
            }
        };
        
        Ok(token_kind)
    }
    
    fn number_literal(&mut self) -> Result<TokenKind<'arena>, LexError> {
        self.buffer.clear();
        
        // Back up to include the first digit
        self.position -= 1;
        self.column -= 1;
        
        // Parse integer part
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.buffer.push(self.advance() as u8);
        }
        
        // Check for decimal point
        if !self.is_at_end() && self.peek() == '.' && 
           self.peek_next().map_or(false, |c| c.is_ascii_digit()) {
            // Consume the '.'
            self.buffer.push(self.advance() as u8);
            
            // Parse fractional part
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                self.buffer.push(self.advance() as u8);
            }
        }
        
        let text = std::str::from_utf8(&self.buffer).unwrap();
        let value = text.parse::<f64>()
            .map_err(|_| LexError::InvalidNumber)?;
        
        Ok(TokenKind::Number(value))
    }
}

#[derive(Debug, Clone)]
pub struct Token<'arena> {
    pub kind: TokenKind<'arena>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum TokenKind<'arena> {
    // Literals
    Number(f64),
    String(&'arena str),
    Identifier(&'arena str),
    
    // Operators
    Plus, Minus, Star, Slash,
    Equal, EqualEqual,
    
    // Delimiters
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    
    // Keywords
    If, Else, While, For,
    Function, Let, Return,
}
```

### High-Performance Type Checker

```rust
use indexmap::IndexMap;
use smallvec::{SmallVec, smallvec};

pub struct TypeChecker {
    // Type definitions with insertion order
    types: IndexMap<TypeId, TypeDefinition>,
    
    // Type inference context
    inference_context: SmallVec<[InferenceVariable; 16]>,
    
    // Constraint solver state
    constraints: SmallVec<[TypeConstraint; 32]>,
    
    // Unification table for type variables
    unification_table: UnificationTable,
}

impl TypeChecker {
    pub fn check_expression(&mut self, expr: &AstNode) -> Result<TypeId, TypeError> {
        match &expr.kind {
            NodeKind::Literal(lit) => self.infer_literal_type(lit),
            NodeKind::Identifier(name) => self.lookup_variable_type(name),
            NodeKind::BinaryOp { op, left, right } => {
                self.check_binary_operation(op, left, right)
            }
            NodeKind::FunctionCall { function, args } => {
                self.check_function_call(function, args)
            }
            _ => todo!("Implement other expression types"),
        }
    }
    
    fn check_binary_operation(
        &mut self, 
        op: &BinaryOperator, 
        left: &AstNode, 
        right: &AstNode
    ) -> Result<TypeId, TypeError> {
        let left_type = self.check_expression(left)?;
        let right_type = self.check_expression(right)?;
        
        match op {
            BinaryOperator::Add | BinaryOperator::Subtract | 
            BinaryOperator::Multiply | BinaryOperator::Divide => {
                // Arithmetic operations require numeric types
                self.unify_types(left_type, right_type)?;
                self.require_numeric_type(left_type)?;
                Ok(left_type)
            }
            BinaryOperator::Equal | BinaryOperator::NotEqual => {
                // Equality operations require compatible types
                self.unify_types(left_type, right_type)?;
                Ok(self.bool_type())
            }
            BinaryOperator::Less | BinaryOperator::Greater |
            BinaryOperator::LessEqual | BinaryOperator::GreaterEqual => {
                // Comparison operations require ordered types
                self.unify_types(left_type, right_type)?;
                self.require_ordered_type(left_type)?;
                Ok(self.bool_type())
            }
        }
    }
    
    fn check_function_call(
        &mut self, 
        function: &AstNode, 
        args: &[&AstNode]
    ) -> Result<TypeId, TypeError> {
        let function_type = self.check_expression(function)?;
        
        // Get function signature
        let signature = self.get_function_signature(function_type)?;
        
        // Check argument count
        if args.len() != signature.parameters.len() {
            return Err(TypeError::ArgumentCountMismatch {
                expected: signature.parameters.len(),
                actual: args.len(),
            });
        }
        
        // Check argument types
        for (arg, param_type) in args.iter().zip(&signature.parameters) {
            let arg_type = self.check_expression(arg)?;
            self.unify_types(arg_type, *param_type)?;
        }
        
        Ok(signature.return_type)
    }
    
    fn unify_types(&mut self, type1: TypeId, type2: TypeId) -> Result<(), TypeError> {
        self.unification_table.unify(type1, type2)
    }
}

// Efficient unification table
pub struct UnificationTable {
    // Parent pointers for union-find
    parents: IndexMap<TypeId, TypeId>,
    
    // Ranks for union-find optimization
    ranks: IndexMap<TypeId, u32>,
    
    // Type substitutions
    substitutions: IndexMap<TypeId, TypeId>,
}

impl UnificationTable {
    pub fn new() -> Self {
        Self {
            parents: IndexMap::new(),
            ranks: IndexMap::new(),
            substitutions: IndexMap::new(),
        }
    }
    
    pub fn unify(&mut self, type1: TypeId, type2: TypeId) -> Result<(), TypeError> {
        let root1 = self.find(type1);
        let root2 = self.find(type2);
        
        if root1 == root2 {
            return Ok(()); // Already unified
        }
        
        // Union by rank
        let rank1 = self.ranks.get(&root1).copied().unwrap_or(0);
        let rank2 = self.ranks.get(&root2).copied().unwrap_or(0);
        
        if rank1 < rank2 {
            self.parents.insert(root1, root2);
        } else if rank1 > rank2 {
            self.parents.insert(root2, root1);
        } else {
            self.parents.insert(root2, root1);
            self.ranks.insert(root1, rank1 + 1);
        }
        
        Ok(())
    }
    
    fn find(&mut self, type_id: TypeId) -> TypeId {
        if let Some(&parent) = self.parents.get(&type_id) {
            if parent != type_id {
                // Path compression
                let root = self.find(parent);
                self.parents.insert(type_id, root);
                root
            } else {
                type_id
            }
        } else {
            // Initialize as its own parent
            self.parents.insert(type_id, type_id);
            type_id
        }
    }
}
```

## LLVM Integration Patterns

### LLVM Code Generation with Memory Optimization

```rust
use bumpalo::Bump;
use indexmap::IndexMap;
use smallvec::{SmallVec, smallvec};

pub struct LLVMCodeGenerator<'arena> {
    arena: &'arena Bump,
    
    // LLVM context and module
    context: llvm::Context,
    module: llvm::Module,
    builder: llvm::Builder,
    
    // Value mappings with insertion order
    values: IndexMap<String, llvm::Value>,
    
    // Function stack for nested functions
    function_stack: SmallVec<[llvm::Function; 8]>,
    
    // Basic block stack
    block_stack: SmallVec<[llvm::BasicBlock; 16]>,
}

impl<'arena> LLVMCodeGenerator<'arena> {
    pub fn new(arena: &'arena Bump, module_name: &str) -> Self {
        let context = llvm::Context::new();
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        
        Self {
            arena,
            context,
            module,
            builder,
            values: IndexMap::new(),
            function_stack: SmallVec::new(),
            block_stack: SmallVec::new(),
        }
    }
    
    pub fn generate_function(&mut self, func: &FunctionDefinition) -> Result<llvm::Function, CodeGenError> {
        // Create function type
        let param_types: SmallVec<[llvm::Type; 8]> = func.parameters
            .iter()
            .map(|param| self.llvm_type_from_type_id(param.type_id))
            .collect::<Result<_, _>>()?;
        
        let return_type = self.llvm_type_from_type_id(func.return_type)?;
        let function_type = self.context.function_type(return_type, &param_types);
        
        // Create function
        let function = self.module.add_function(&func.name, function_type);
        self.function_stack.push(function);
        
        // Create entry block
        let entry_block = self.context.append_basic_block(function, "entry");
        self.block_stack.push(entry_block);
        self.builder.position_at_end(entry_block);
        
        // Generate parameter bindings
        for (i, param) in func.parameters.iter().enumerate() {
            let llvm_param = function.get_param(i);
            llvm_param.set_name(&param.name);
            self.values.insert(param.name.clone(), llvm_param);
        }
        
        // Generate function body
        self.generate_statement(&func.body)?;
        
        // Clean up stacks
        self.function_stack.pop();
        self.block_stack.pop();
        
        Ok(function)
    }
    
    pub fn generate_expression(&mut self, expr: &AstNode) -> Result<llvm::Value, CodeGenError> {
        match &expr.kind {
            NodeKind::Literal(lit) => self.generate_literal(lit),
            NodeKind::Identifier(name) => {
                self.values.get(*name)
                    .copied()
                    .ok_or_else(|| CodeGenError::UndefinedVariable(name.to_string()))
            }
            NodeKind::BinaryOp { op, left, right } => {
                self.generate_binary_operation(op, left, right)
            }
            NodeKind::FunctionCall { function, args } => {
                self.generate_function_call(function, args)
            }
            _ => todo!("Implement other expression types"),
        }
    }
    
    fn generate_binary_operation(
        &mut self,
        op: &BinaryOperator,
        left: &AstNode,
        right: &AstNode,
    ) -> Result<llvm::Value, CodeGenError> {
        let left_val = self.generate_expression(left)?;
        let right_val = self.generate_expression(right)?;
        
        let result = match op {
            BinaryOperator::Add => self.builder.build_add(left_val, right_val, "add"),
            BinaryOperator::Subtract => self.builder.build_sub(left_val, right_val, "sub"),
            BinaryOperator::Multiply => self.builder.build_mul(left_val, right_val, "mul"),
            BinaryOperator::Divide => self.builder.build_div(left_val, right_val, "div"),
            BinaryOperator::Equal => self.builder.build_icmp_eq(left_val, right_val, "eq"),
            BinaryOperator::NotEqual => self.builder.build_icmp_ne(left_val, right_val, "ne"),
            BinaryOperator::Less => self.builder.build_icmp_slt(left_val, right_val, "lt"),
            BinaryOperator::Greater => self.builder.build_icmp_sgt(left_val, right_val, "gt"),
            BinaryOperator::LessEqual => self.builder.build_icmp_sle(left_val, right_val, "le"),
            BinaryOperator::GreaterEqual => self.builder.build_icmp_sge(left_val, right_val, "ge"),
        };
        
        Ok(result)
    }
    
    fn generate_function_call(
        &mut self,
        function: &AstNode,
        args: &[&AstNode],
    ) -> Result<llvm::Value, CodeGenError> {
        let function_val = self.generate_expression(function)?;
        
        // Generate argument values
        let arg_values: SmallVec<[llvm::Value; 8]> = args
            .iter()
            .map(|arg| self.generate_expression(arg))
            .collect::<Result<_, _>>()?;
        
        let call = self.builder.build_call(function_val, &arg_values, "call");
        Ok(call)
    }
    
    pub fn optimize_module(&mut self) {
        // Run LLVM optimization passes
        let pass_manager = llvm::PassManager::new();
        
        // Add optimization passes
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_cfg_simplification_pass();
        pass_manager.add_dead_code_elimination_pass();
        
        // Run passes on module
        pass_manager.run_on_module(&mut self.module);
    }
    
    pub fn emit_object_file(&self, filename: &str) -> Result<(), CodeGenError> {
        // Emit object file
        self.module.emit_to_file(filename, llvm::FileType::Object)
            .map_err(|e| CodeGenError::LLVMError(e.to_string()))
    }
}

// Placeholder types for LLVM integration
mod llvm {
    pub struct Context;
    pub struct Module;
    pub struct Builder;
    pub struct Function;
    pub struct BasicBlock;
    pub struct Value;
    pub struct Type;
    pub struct PassManager;
    pub enum FileType { Object }
    
    impl Context {
        pub fn new() -> Self { Self }
        pub fn create_module(&self, _name: &str) -> Module { Module }
        pub fn create_builder(&self) -> Builder { Builder }
        pub fn function_type(&self, _ret: Type, _params: &[Type]) -> Type { Type }
        pub fn append_basic_block(&self, _func: Function, _name: &str) -> BasicBlock { BasicBlock }
    }
    
    impl Module {
        pub fn add_function(&self, _name: &str, _ty: Type) -> Function { Function }
        pub fn emit_to_file(&self, _filename: &str, _ty: FileType) -> Result<(), String> { Ok(()) }
    }
    
    impl Builder {
        pub fn position_at_end(&self, _block: BasicBlock) {}
        pub fn build_add(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_sub(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_mul(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_div(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_icmp_eq(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_icmp_ne(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_icmp_slt(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_icmp_sgt(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_icmp_sle(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_icmp_sge(&self, _lhs: Value, _rhs: Value, _name: &str) -> Value { Value }
        pub fn build_call(&self, _func: Value, _args: &[Value], _name: &str) -> Value { Value }
    }
    
    impl Function {
        pub fn get_param(&self, _index: usize) -> Value { Value }
    }
    
    impl Value {
        pub fn set_name(&self, _name: &str) {}
    }
    
    impl PassManager {
        pub fn new() -> Self { Self }
        pub fn add_instruction_combining_pass(&self) {}
        pub fn add_reassociate_pass(&self) {}
        pub fn add_gvn_pass(&self) {}
        pub fn add_cfg_simplification_pass(&self) {}
        pub fn add_dead_code_elimination_pass(&self) {}
        pub fn run_on_module(&self, _module: &mut Module) {}
    }
}
```

## Parallel Compilation Techniques

### Work-Stealing Compilation

```rust
use std::sync::{Arc, Mutex};
use std::thread;
use crossbeam::deque::{Injector, Stealer, Worker};
use smallvec::{SmallVec, smallvec};

pub struct ParallelCompiler {
    // Global work queue
    global_queue: Arc<Injector<CompilationTask>>,
    
    // Per-thread work queues
    workers: SmallVec<[Worker<CompilationTask>; 8]>,
    stealers: SmallVec<[Stealer<CompilationTask>; 8]>,
    
    // Shared compilation context
    context: Arc<Mutex<CompilationContext>>,
    
    // Thread pool
    thread_pool: SmallVec<[thread::JoinHandle<()>; 8]>,
}

#[derive(Debug)]
pub enum CompilationTask {
    ParseModule {
        module_id: ModuleId,
        source: String,
    },
    TypeCheckModule {
        module_id: ModuleId,
        ast: Arc<AstNode>,
    },
    GenerateCode {
        module_id: ModuleId,
        typed_ast: Arc<TypedAstNode>,
    },
    OptimizeModule {
        module_id: ModuleId,
        ir: Arc<IntermediateRepresentation>,
    },
}

impl ParallelCompiler {
    pub fn new(num_threads: usize) -> Self {
        let global_queue = Arc::new(Injector::new());
        let mut workers = SmallVec::new();
        let mut stealers = SmallVec::new();
        
        for _ in 0..num_threads {
            let worker = Worker::new_fifo();
            let stealer = worker.stealer();
            workers.push(worker);
            stealers.push(stealer);
        }
        
        Self {
            global_queue,
            workers,
            stealers,
            context: Arc::new(Mutex::new(CompilationContext::new())),
            thread_pool: SmallVec::new(),
        }
    }
    
    pub fn compile_project(&mut self, project: &Project) -> Result<CompiledProject, CompileError> {
        // Schedule initial parsing tasks
        for module in &project.modules {
            let task = CompilationTask::ParseModule {
                module_id: module.id,
                source: module.source.clone(),
            };
            self.global_queue.push(task);
        }
        
        // Start worker threads
        self.start_workers();
        
        // Wait for compilation to complete
        self.wait_for_completion();
        
        // Collect results
        let context = self.context.lock().unwrap();
        Ok(context.build_project())
    }
    
    fn start_workers(&mut self) {
        let num_workers = self.workers.len();
        
        for i in 0..num_workers {
            let worker = std::mem::replace(&mut self.workers[i], Worker::new_fifo());
            let global_queue = Arc::clone(&self.global_queue);
            let stealers = self.stealers.clone();
            let context = Arc::clone(&self.context);
            
            let handle = thread::spawn(move || {
                Self::worker_thread(worker, global_queue, stealers, context);
            });
            
            self.thread_pool.push(handle);
        }
    }
    
    fn worker_thread(
        worker: Worker<CompilationTask>,
        global_queue: Arc<Injector<CompilationTask>>,
        stealers: SmallVec<[Stealer<CompilationTask>; 8]>,
        context: Arc<Mutex<CompilationContext>>,
    ) {
        loop {
            // Try to get work from local queue first
            let task = worker.pop()
                .or_else(|| {
                    // Try to steal from global queue
                    global_queue.steal().success()
                })
                .or_else(|| {
                    // Try to steal from other workers
                    stealers.iter()
                        .map(|stealer| stealer.steal())
                        .find_map(|steal| steal.success())
                });
            
            match task {
                Some(task) => {
                    let follow_up_tasks = Self::execute_task(task, &context);
                    
                    // Schedule follow-up tasks
                    for follow_up in follow_up_tasks {
                        worker.push(follow_up);
                    }
                }
                None => {
                    // No work available, check if compilation is complete
                    if Self::is_compilation_complete(&context) {
                        break;
                    }
                    
                    // Brief pause before trying again
                    thread::yield_now();
                }
            }
        }
    }
    
    fn execute_task(
        task: CompilationTask,
        context: &Arc<Mutex<CompilationContext>>,
    ) -> SmallVec<[CompilationTask; 4]> {
        let mut follow_up_tasks = SmallVec::new();
        
        match task {
            CompilationTask::ParseModule { module_id, source } => {
                // Parse the module
                let arena = bumpalo::Bump::new();
                let mut parser = ArenaParser::new(&arena, &source);
                
                match parser.parse() {
                    Ok(ast) => {
                        let ast = Arc::new(ast.clone()); // Clone out of arena
                        
                        // Update context
                        {
                            let mut ctx = context.lock().unwrap();
                            ctx.set_module_ast(module_id, ast.clone());
                        }
                        
                        // Schedule type checking
                        follow_up_tasks.push(CompilationTask::TypeCheckModule {
                            module_id,
                            ast,
                        });
                    }
                    Err(error) => {
                        let mut ctx = context.lock().unwrap();
                        ctx.add_error(module_id, error);
                    }
                }
            }
            
            CompilationTask::TypeCheckModule { module_id, ast } => {
                // Type check the module
                let mut type_checker = TypeChecker::new();
                
                match type_checker.check_module(&ast) {
                    Ok(typed_ast) => {
                        let typed_ast = Arc::new(typed_ast);
                        
                        // Update context
                        {
                            let mut ctx = context.lock().unwrap();
                            ctx.set_module_typed_ast(module_id, typed_ast.clone());
                        }
                        
                        // Schedule code generation
                        follow_up_tasks.push(CompilationTask::GenerateCode {
                            module_id,
                            typed_ast,
                        });
                    }
                    Err(error) => {
                        let mut ctx = context.lock().unwrap();
                        ctx.add_error(module_id, error);
                    }
                }
            }
            
            CompilationTask::GenerateCode { module_id, typed_ast } => {
                // Generate code for the module
                let arena = bumpalo::Bump::new();
                let mut code_gen = LLVMCodeGenerator::new(&arena, "module");
                
                match code_gen.generate_module(&typed_ast) {
                    Ok(ir) => {
                        let ir = Arc::new(ir);
                        
                        // Update context
                        {
                            let mut ctx = context.lock().unwrap();
                            ctx.set_module_ir(module_id, ir.clone());
                        }
                        
                        // Schedule optimization
                        follow_up_tasks.push(CompilationTask::OptimizeModule {
                            module_id,
                            ir,
                        });
                    }
                    Err(error) => {
                        let mut ctx = context.lock().unwrap();
                        ctx.add_error(module_id, error);
                    }
                }
            }
            
            CompilationTask::OptimizeModule { module_id, ir } => {
                // Optimize the module
                let mut optimizer = Optimizer::new();
                
                match optimizer.optimize(&ir) {
                    Ok(optimized_ir) => {
                        let mut ctx = context.lock().unwrap();
                        ctx.set_module_optimized_ir(module_id, optimized_ir);
                        ctx.mark_module_complete(module_id);
                    }
                    Err(error) => {
                        let mut ctx = context.lock().unwrap();
                        ctx.add_error(module_id, error);
                    }
                }
            }
        }
        
        follow_up_tasks
    }
}

// Shared compilation state
pub struct CompilationContext {
    modules: IndexMap<ModuleId, ModuleCompilationState>,
    errors: SmallVec<[CompileError; 16]>,
}

#[derive(Debug)]
pub struct ModuleCompilationState {
    pub ast: Option<Arc<AstNode>>,
    pub typed_ast: Option<Arc<TypedAstNode>>,
    pub ir: Option<Arc<IntermediateRepresentation>>,
    pub optimized_ir: Option<OptimizedIR>,
    pub complete: bool,
}

impl CompilationContext {
    pub fn new() -> Self {
        Self {
            modules: IndexMap::new(),
            errors: SmallVec::new(),
        }
    }
    
    pub fn set_module_ast(&mut self, module_id: ModuleId, ast: Arc<AstNode>) {
        let state = self.modules.entry(module_id).or_insert_with(|| ModuleCompilationState {
            ast: None,
            typed_ast: None,
            ir: None,
            optimized_ir: None,
            complete: false,
        });
        state.ast = Some(ast);
    }
    
    pub fn mark_module_complete(&mut self, module_id: ModuleId) {
        if let Some(state) = self.modules.get_mut(&module_id) {
            state.complete = true;
        }
    }
    
    pub fn is_complete(&self) -> bool {
        self.modules.values().all(|state| state.complete || !self.errors.is_empty())
    }
}
```

## Incremental Compilation Support

### Dependency Tracking with IndexMap

```rust
use indexmap::{IndexMap, IndexSet};
use smallvec::{SmallVec, smallvec};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub struct IncrementalCompiler {
    // Module dependency graph (preserves order)
    dependencies: IndexMap<ModuleId, IndexSet<ModuleId>>,
    
    // Module hashes for change detection
    module_hashes: IndexMap<ModuleId, u64>,
    
    // Compilation artifacts cache
    artifact_cache: IndexMap<ModuleId, CachedArtifacts>,
    
    // Invalidation queue
    invalidated_modules: IndexSet<ModuleId>,
}

#[derive(Debug, Clone)]
pub struct CachedArtifacts {
    pub ast: Option<Arc<AstNode>>,
    pub typed_ast: Option<Arc<TypedAstNode>>,
    pub ir: Option<Arc<IntermediateRepresentation>>,
    pub timestamp: std::time::SystemTime,
    pub dependencies_hash: u64,
}

impl IncrementalCompiler {
    pub fn new() -> Self {
        Self {
            dependencies: IndexMap::new(),
            module_hashes: IndexMap::new(),
            artifact_cache: IndexMap::new(),
            invalidated_modules: IndexSet::new(),
        }
    }
    
    pub fn compile_project_incremental(
        &mut self, 
        project: &Project
    ) -> Result<CompiledProject, CompileError> {
        // Detect changed modules
        self.detect_changes(project);
        
        // Build dependency-ordered compilation list
        let compilation_order = self.compute_compilation_order()?;
        
        // Compile only what's necessary
        for module_id in compilation_order {
            if self.needs_recompilation(module_id) {
                self.compile_module(module_id, project)?;
            }
        }
        
        // Build final project
        self.build_project()
    }
    
    fn detect_changes(&mut self, project: &Project) {
        for module in &project.modules {
            let current_hash = self.compute_module_hash(&module.source);
            
            if let Some(&previous_hash) = self.module_hashes.get(&module.id) {
                if current_hash != previous_hash {
                    // Module changed, invalidate it and dependents
                    self.invalidate_module(module.id);
                }
            } else {
                // New module
                self.invalidated_modules.insert(module.id);
            }
            
            self.module_hashes.insert(module.id, current_hash);
        }
    }
    
    fn invalidate_module(&mut self, module_id: ModuleId) {
        self.invalidated_modules.insert(module_id);
        
        // Invalidate all modules that depend on this one
        let dependents = self.find_dependents(module_id);
        for dependent in dependents {
            self.invalidate_module(dependent);
        }
    }
    
    fn find_dependents(&self, module_id: ModuleId) -> SmallVec<[ModuleId; 8]> {
        let mut dependents = SmallVec::new();
        
        for (&dependent_id, dependencies) in &self.dependencies {
            if dependencies.contains(&module_id) {
                dependents.push(dependent_id);
            }
        }
        
        dependents
    }
    
    fn compute_compilation_order(&self) -> Result<Vec<ModuleId>, CompileError> {
        // Topological sort of dependency graph
        let mut order = Vec::new();
        let mut visited = IndexSet::new();
        let mut visiting = IndexSet::new();
        
        for &module_id in self.dependencies.keys() {
            if !visited.contains(&module_id) {
                self.topological_sort_visit(
                    module_id, 
                    &mut order, 
                    &mut visited, 
                    &mut visiting
                )?;
            }
        }
        
        Ok(order)
    }
    
    fn topological_sort_visit(
        &self,
        module_id: ModuleId,
        order: &mut Vec<ModuleId>,
        visited: &mut IndexSet<ModuleId>,
        visiting: &mut IndexSet<ModuleId>,
    ) -> Result<(), CompileError> {
        if visiting.contains(&module_id) {
            return Err(CompileError::CircularDependency(module_id));
        }
        
        if visited.contains(&module_id) {
            return Ok(());
        }
        
        visiting.insert(module_id);
        
        if let Some(dependencies) = self.dependencies.get(&module_id) {
            for &dep_id in dependencies {
                self.topological_sort_visit(dep_id, order, visited, visiting)?;
            }
        }
        
        visiting.remove(&module_id);
        visited.insert(module_id);
        order.push(module_id);
        
        Ok(())
    }
    
    fn needs_recompilation(&self, module_id: ModuleId) -> bool {
        // Check if module is invalidated
        if self.invalidated_modules.contains(&module_id) {
            return true;
        }
        
        // Check if dependencies changed
        if let Some(cached) = self.artifact_cache.get(&module_id) {
            let current_deps_hash = self.compute_dependencies_hash(module_id);
            if current_deps_hash != cached.dependencies_hash {
                return true;
            }
        } else {
            // No cached artifacts
            return true;
        }
        
        false
    }
    
    fn compute_module_hash(&self, source: &str) -> u64 {
        let mut hasher = DefaultHasher::new();
        source.hash(&mut hasher);
        hasher.finish()
    }
    
    fn compute_dependencies_hash(&self, module_id: ModuleId) -> u64 {
        let mut hasher = DefaultHasher::new();
        
        if let Some(dependencies) = self.dependencies.get(&module_id) {
            for &dep_id in dependencies {
                if let Some(&dep_hash) = self.module_hashes.get(&dep_id) {
                    dep_hash.hash(&mut hasher);
                }
            }
        }
        
        hasher.finish()
    }
    
    fn compile_module(
        &mut self, 
        module_id: ModuleId, 
        project: &Project
    ) -> Result<(), CompileError> {
        let module = project.get_module(module_id)
            .ok_or(CompileError::ModuleNotFound(module_id))?;
        
        // Check cache first
        if let Some(cached) = self.artifact_cache.get(&module_id) {
            if !self.needs_recompilation(module_id) {
                return Ok(()); // Use cached artifacts
            }
        }
        
        // Compile the module
        let arena = bumpalo::Bump::new();
        
        // Parse
        let mut parser = ArenaParser::new(&arena, &module.source);
        let ast = parser.parse()?;
        let ast = Arc::new(ast.clone()); // Clone out of arena
        
        // Type check
        let mut type_checker = TypeChecker::new();
        let typed_ast = type_checker.check_module(&ast)?;
        let typed_ast = Arc::new(typed_ast);
        
        // Generate IR
        let mut code_gen = LLVMCodeGenerator::new(&arena, &module.name);
        let ir = code_gen.generate_module(&typed_ast)?;
        let ir = Arc::new(ir);
        
        // Cache artifacts
        let artifacts = CachedArtifacts {
            ast: Some(ast),
            typed_ast: Some(typed_ast),
            ir: Some(ir),
            timestamp: std::time::SystemTime::now(),
            dependencies_hash: self.compute_dependencies_hash(module_id),
        };
        
        self.artifact_cache.insert(module_id, artifacts);
        self.invalidated_modules.remove(&module_id);
        
        Ok(())
    }
}

// Dependency analysis
pub struct DependencyAnalyzer {
    current_module: Option<ModuleId>,
    dependencies: IndexMap<ModuleId, IndexSet<ModuleId>>,
}

impl DependencyAnalyzer {
    pub fn new() -> Self {
        Self {
            current_module: None,
            dependencies: IndexMap::new(),
        }
    }
    
    pub fn analyze_module(&mut self, module_id: ModuleId, ast: &AstNode) {
        self.current_module = Some(module_id);
        self.dependencies.insert(module_id, IndexSet::new());
        
        self.visit_node(ast);
    }
    
    fn visit_node(&mut self, node: &AstNode) {
        match &node.kind {
            NodeKind::Import { module_name } => {
                if let Some(current) = self.current_module {
                    let imported_module = self.resolve_module_name(module_name);
                    self.dependencies
                        .get_mut(&current)
                        .unwrap()
                        .insert(imported_module);
                }
            }
            _ => {
                // Visit child nodes
                self.visit_children(node);
            }
        }
    }
    
    fn visit_children(&mut self, node: &AstNode) {
        match &node.kind {
            NodeKind::Block { statements } => {
                for stmt in *statements {
                    self.visit_node(stmt);
                }
            }
            NodeKind::BinaryOp { left, right, .. } => {
                self.visit_node(left);
                self.visit_node(right);
            }
            NodeKind::FunctionCall { function, args } => {
                self.visit_node(function);
                for arg in *args {
                    self.visit_node(arg);
                }
            }
            _ => {} // Other node types
        }
    }
    
    fn resolve_module_name(&self, name: &str) -> ModuleId {
        // Resolve module name to ModuleId
        // This would involve looking up the module in the project
        ModuleId(name.hash(&mut DefaultHasher::new()) as u32)
    }
    
    pub fn get_dependencies(&self) -> &IndexMap<ModuleId, IndexSet<ModuleId>> {
        &self.dependencies
    }
}
```

## Target-Specific Optimizations

### Architecture-Specific Code Generation

```rust
use smallvec::{SmallVec, smallvec};

pub struct TargetSpecificOptimizer {
    target_arch: TargetArchitecture,
    target_features: SmallVec<[TargetFeature; 8]>,
    optimization_level: OptimizationLevel,
}

#[derive(Debug, Clone, Copy)]
pub enum TargetArchitecture {
    X86_64,
    AArch64,
    RISCV64,
    WASM32,
}

#[derive(Debug, Clone, Copy)]
pub enum TargetFeature {
    AVX2,
    AVX512,
    NEON,
    SVE,
    SIMD128,
}

#[derive(Debug, Clone, Copy)]
pub enum OptimizationLevel {
    Debug,
    Release,
    ReleaseFast,
    ReleaseSmall,
}

impl TargetSpecificOptimizer {
    pub fn new(
        target_arch: TargetArchitecture,
        target_features: SmallVec<[TargetFeature; 8]>,
        optimization_level: OptimizationLevel,
    ) -> Self {
        Self {
            target_arch,
            target_features,
            optimization_level,
        }
    }
    
    pub fn optimize_ir(&self, ir: &mut IntermediateRepresentation) {
        match self.target_arch {
            TargetArchitecture::X86_64 => self.optimize_x86_64(ir),
            TargetArchitecture::AArch64 => self.optimize_aarch64(ir),
            TargetArchitecture::RISCV64 => self.optimize_riscv64(ir),
            TargetArchitecture::WASM32 => self.optimize_wasm32(ir),
        }
    }
    
    fn optimize_x86_64(&self, ir: &mut IntermediateRepresentation) {
        // X86-64 specific optimizations
        
        if self.target_features.contains(&TargetFeature::AVX2) {
            self.vectorize_loops_avx2(ir);
        }
        
        if self.target_features.contains(&TargetFeature::AVX512) {
            self.vectorize_loops_avx512(ir);
        }
        
        // Optimize for x86-64 calling conventions
        self.optimize_calling_conventions_x86_64(ir);
        
        // Use x86-64 specific instructions
        self.use_x86_64_instructions(ir);
    }
    
    fn optimize_aarch64(&self, ir: &mut IntermediateRepresentation) {
        // AArch64 specific optimizations
        
        if self.target_features.contains(&TargetFeature::NEON) {
            self.vectorize_loops_neon(ir);
        }
        
        if self.target_features.contains(&TargetFeature::SVE) {
            self.vectorize_loops_sve(ir);
        }
        
        // Optimize for AArch64 calling conventions
        self.optimize_calling_conventions_aarch64(ir);
    }
    
    fn vectorize_loops_avx2(&self, ir: &mut IntermediateRepresentation) {
        for function in &mut ir.functions {
            for block in &mut function.basic_blocks {
                for instruction in &mut block.instructions {
                    if let Instruction::Loop { body, .. } = instruction {
                        self.try_vectorize_loop_avx2(body);
                    }
                }
            }
        }
    }
    
    fn try_vectorize_loop_avx2(&self, loop_body: &mut LoopBody) {
        // Analyze loop for vectorization opportunities
        if self.can_vectorize_loop(loop_body) {
            // Transform scalar operations to AVX2 vector operations
            self.transform_to_avx2_vectors(loop_body);
        }
    }
    
    fn can_vectorize_loop(&self, loop_body: &LoopBody) -> bool {
        // Check if loop can be safely vectorized
        // - No dependencies between iterations
        // - Simple arithmetic operations
        // - Contiguous memory access patterns
        
        for instruction in &loop_body.instructions {
            match instruction {
                Instruction::Load { address, .. } => {
                    if !self.is_contiguous_access(address) {
                        return false;
                    }
                }
                Instruction::Store { address, .. } => {
                    if !self.is_contiguous_access(address) {
                        return false;
                    }
                }
                Instruction::Call { .. } => {
                    // Function calls prevent vectorization
                    return false;
                }
                _ => {}
            }
        }
        
        true
    }
    
    fn transform_to_avx2_vectors(&self, loop_body: &mut LoopBody) {
        // Transform scalar operations to 256-bit vector operations
        let mut new_instructions = SmallVec::new();
        
        for instruction in &loop_body.instructions {
            match instruction {
                Instruction::Add { dest, src1, src2 } => {
                    // Transform to vector add
                    new_instructions.push(Instruction::VectorAdd {
                        dest: *dest,
                        src1: *src1,
                        src2: *src2,
                        vector_width: 256,
                    });
                }
                Instruction::Multiply { dest, src1, src2 } => {
                    // Transform to vector multiply
                    new_instructions.push(Instruction::VectorMultiply {
                        dest: *dest,
                        src1: *src1,
                        src2: *src2,
                        vector_width: 256,
                    });
                }
                _ => {
                    new_instructions.push(instruction.clone());
                }
            }
        }
        
        loop_body.instructions = new_instructions;
    }
}

// Target-specific instruction selection
pub struct InstructionSelector {
    target_arch: TargetArchitecture,
    instruction_costs: IndexMap<InstructionPattern, u32>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum InstructionPattern {
    Add,
    Multiply,
    Divide,
    Load,
    Store,
    Branch,
    VectorAdd(u32), // vector width
    VectorMultiply(u32),
}

impl InstructionSelector {
    pub fn new(target_arch: TargetArchitecture) -> Self {
        let mut instruction_costs = IndexMap::new();
        
        match target_arch {
            TargetArchitecture::X86_64 => {
                instruction_costs.insert(InstructionPattern::Add, 1);
                instruction_costs.insert(InstructionPattern::Multiply, 3);
                instruction_costs.insert(InstructionPattern::Divide, 20);
                instruction_costs.insert(InstructionPattern::Load, 3);
                instruction_costs.insert(InstructionPattern::Store, 1);
                instruction_costs.insert(InstructionPattern::VectorAdd(256), 1);
                instruction_costs.insert(InstructionPattern::VectorMultiply(256), 3);
            }
            TargetArchitecture::AArch64 => {
                instruction_costs.insert(InstructionPattern::Add, 1);
                instruction_costs.insert(InstructionPattern::Multiply, 2);
                instruction_costs.insert(InstructionPattern::Divide, 15);
                instruction_costs.insert(InstructionPattern::Load, 2);
                instruction_costs.insert(InstructionPattern::Store, 1);
                instruction_costs.insert(InstructionPattern::VectorAdd(128), 1);
                instruction_costs.insert(InstructionPattern::VectorMultiply(128), 2);
            }
            _ => {
                // Default costs
                instruction_costs.insert(InstructionPattern::Add, 1);
                instruction_costs.insert(InstructionPattern::Multiply, 4);
                instruction_costs.insert(InstructionPattern::Divide, 25);
            }
        }
        
        Self {
            target_arch,
            instruction_costs,
        }
    }
    
    pub fn select_best_instruction(&self, operation: &Operation) -> Instruction {
        match operation {
            Operation::Add { dest, src1, src2 } => {
                // Check if vectorization is beneficial
                if self.should_vectorize_add(src1, src2) {
                    let vector_width = self.get_optimal_vector_width();
                    Instruction::VectorAdd {
                        dest: *dest,
                        src1: *src1,
                        src2: *src2,
                        vector_width,
                    }
                } else {
                    Instruction::Add {
                        dest: *dest,
                        src1: *src1,
                        src2: *src2,
                    }
                }
            }
            _ => todo!("Implement other operations"),
        }
    }
    
    fn should_vectorize_add(&self, _src1: &Operand, _src2: &Operand) -> bool {
        // Determine if vectorization is beneficial
        match self.target_arch {
            TargetArchitecture::X86_64 => true, // AVX2 available
            TargetArchitecture::AArch64 => true, // NEON available
            _ => false,
        }
    }
    
    fn get_optimal_vector_width(&self) -> u32 {
        match self.target_arch {
            TargetArchitecture::X86_64 => 256, // AVX2
            TargetArchitecture::AArch64 => 128, // NEON
            _ => 128,
        }
    }
}
```

## Runtime Integration

### Runtime System Integration

```rust
use bumpalo::Bump;
use smallvec::{SmallVec, smallvec};
use indexmap::IndexMap;

pub struct ParallaxRuntime {
    // Work-stealing scheduler
    scheduler: WorkStealingScheduler,
    
    // Memory management
    heap_manager: HeapManager,
    
    // Function registry
    functions: IndexMap<FunctionId, CompiledFunction>,
    
    // Runtime statistics
    stats: RuntimeStats,
}

pub struct WorkStealingScheduler {
    // Worker threads
    workers: SmallVec<[WorkerThread; 16]>,
    
    // Global work queue
    global_queue: crossbeam::deque::Injector<WorkItem>,
    
    // Thread-local work queues
    local_queues: SmallVec<[crossbeam::deque::Worker<WorkItem>; 16]>,
}

#[derive(Debug)]
pub enum WorkItem {
    // Direct computation result
    Value(RuntimeValue),
    
    // Parallel work opportunity
    Parallel {
        function_id: FunctionId,
        args: SmallVec<[RuntimeValue; 4]>,
        continuation: ContinuationId,
    },
    
    // Continuation to resume
    Continuation {
        continuation_id: ContinuationId,
        result: RuntimeValue,
    },
}

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Array(Vec<RuntimeValue>),
    Function(FunctionId),
}

impl ParallaxRuntime {
    pub fn new(num_workers: usize) -> Self {
        Self {
            scheduler: WorkStealingScheduler::new(num_workers),
            heap_manager: HeapManager::new(),
            functions: IndexMap::new(),
            stats: RuntimeStats::new(),
        }
    }
    
    pub fn register_function(&mut self, id: FunctionId, function: CompiledFunction) {
        self.functions.insert(id, function);
    }
    
    pub fn execute_function(
        &mut self, 
        function_id: FunctionId, 
        args: SmallVec<[RuntimeValue; 4]>
    ) -> Result<RuntimeValue, RuntimeError> {
        // Create initial work item
        let work_item = WorkItem::Parallel {
            function_id,
            args,
            continuation: ContinuationId::root(),
        };
        
        // Submit to scheduler
        self.scheduler.submit_work(work_item);
        
        // Run until completion
        self.scheduler.run_until_complete()
    }
}

impl WorkStealingScheduler {
    pub fn new(num_workers: usize) -> Self {
        let global_queue = crossbeam::deque::Injector::new();
        let mut local_queues = SmallVec::new();
        let mut workers = SmallVec::new();
        
        for i in 0..num_workers {
            let local_queue = crossbeam::deque::Worker::new_fifo();
            let worker = WorkerThread::new(i, local_queue.stealer());
            
            local_queues.push(local_queue);
            workers.push(worker);
        }
        
        Self {
            workers,
            global_queue,
            local_queues,
        }
    }
    
    pub fn submit_work(&self, work_item: WorkItem) {
        // Try to submit to a local queue first
        if let Some(local_queue) = self.local_queues.first() {
            local_queue.push(work_item);
        } else {
            // Fall back to global queue
            self.global_queue.push(work_item);
        }
    }
    
    pub fn run_until_complete(&mut self) -> Result<RuntimeValue, RuntimeError> {
        // Start worker threads
        let handles = self.start_workers();
        
        // Wait for completion
        for handle in handles {
            handle.join().unwrap();
        }
        
        // Return result
        todo!("Implement result collection")
    }
    
    fn start_workers(&self) -> SmallVec<[std::thread::JoinHandle<()>; 16]> {
        let mut handles = SmallVec::new();
        
        for (i, worker) in self.workers.iter().enumerate() {
            let local_queue = self.local_queues[i].clone();
            let global_queue = self.global_queue.clone();
            let stealers: SmallVec<_> = self.local_queues
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != i)
                .map(|(_, queue)| queue.stealer())
                .collect();
            
            let handle = std::thread::spawn(move || {
                Self::worker_loop(local_queue, global_queue, stealers);
            });
            
            handles.push(handle);
        }
        
        handles
    }
    
    fn worker_loop(
        local_queue: crossbeam::deque::Worker<WorkItem>,
        global_queue: crossbeam::deque::Injector<WorkItem>,
        stealers: SmallVec<[crossbeam::deque::Stealer<WorkItem>; 16]>,
    ) {
        loop {
            // Try to get work
            let work_item = local_queue.pop()
                .or_else(|| global_queue.steal().success())
                .or_else(|| {
                    stealers.iter()
                        .map(|stealer| stealer.steal())
                        .find_map(|steal| steal.success())
                });
            
            match work_item {
                Some(work_item) => {
                    Self::execute_work_item(work_item, &local_queue);
                }
                None => {
                    // No work available, check for termination
                    if Self::should_terminate() {
                        break;
                    }
                    
                    std::thread::yield_now();
                }
            }
        }
    }
    
    fn execute_work_item(
        work_item: WorkItem, 
        local_queue: &crossbeam::deque::Worker<WorkItem>
    ) {
        match work_item {
            WorkItem::Value(value) => {
                // Direct value, nothing to do
                println!("Computed value: {:?}", value);
            }
            
            WorkItem::Parallel { function_id, args, continuation } => {
                // Execute function and handle result
                let result = Self::call_function(function_id, args);
                
                match result {
                    Ok(WorkItem::Value(value)) => {
                        // Function returned a direct value
                        let cont_item = WorkItem::Continuation {
                            continuation_id: continuation,
                            result: value,
                        };
                        local_queue.push(cont_item);
                    }
                    Ok(parallel_work) => {
                        // Function returned more parallel work
                        local_queue.push(parallel_work);
                    }
                    Err(error) => {
                        eprintln!("Function execution error: {:?}", error);
                    }
                }
            }
            
            WorkItem::Continuation { continuation_id, result } => {
                // Resume continuation with result
                Self::resume_continuation(continuation_id, result, local_queue);
            }
        }
    }
    
    fn call_function(
        function_id: FunctionId, 
        args: SmallVec<[RuntimeValue; 4]>
    ) -> Result<WorkItem, RuntimeError> {
        // This would call the actual compiled function
        // For now, simulate some computation
        
        match function_id.0 {
            0 => {
                // Simple addition function
                if args.len() == 2 {
                    if let (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) = (&args[0], &args[1]) {
                        Ok(WorkItem::Value(RuntimeValue::Integer(a + b)))
                    } else {
                        Err(RuntimeError::TypeMismatch)
                    }
                } else {
                    Err(RuntimeError::ArgumentCountMismatch)
                }
            }
            1 => {
                // Parallel map function
                if let RuntimeValue::Array(array) = &args[0] {
                    // Create parallel work for each element
                    // This is a simplified example
                    Ok(WorkItem::Value(RuntimeValue::Array(array.clone())))
                } else {
                    Err(RuntimeError::TypeMismatch)
                }
            }
            _ => Err(RuntimeError::UnknownFunction(function_id)),
        }
    }
}

// Memory management for runtime
pub struct HeapManager {
    // Arena for temporary allocations
    temp_arena: Bump,
    
    // Garbage collector state
    gc_state: GCState,
    
    // Allocation statistics
    allocation_stats: AllocationStats,
}

#[derive(Debug)]
pub struct GCState {
    // Objects to be collected
    to_collect: SmallVec<[ObjectId; 256]>,
    
    // Root set
    roots: SmallVec<[ObjectId; 64]>,
    
    // Collection threshold
    collection_threshold: usize,
}

impl HeapManager {
    pub fn new() -> Self {
        Self {
            temp_arena: Bump::new(),
            gc_state: GCState {
                to_collect: SmallVec::new(),
                roots: SmallVec::new(),
                collection_threshold: 1024 * 1024, // 1MB
            },
            allocation_stats: AllocationStats::new(),
        }
    }
    
    pub fn allocate_temp<T>(&self, value: T) -> &T {
        self.temp_arena.alloc(value)
    }
    
    pub fn reset_temp_arena(&mut self) {
        self.temp_arena.reset();
    }
    
    pub fn maybe_collect(&mut self) {
        if self.allocation_stats.bytes_allocated > self.gc_state.collection_threshold {
            self.collect_garbage();
        }
    }
    
    fn collect_garbage(&mut self) {
        // Mark and sweep garbage collection
        self.mark_phase();
        self.sweep_phase();
        
        // Reset allocation counter
        self.allocation_stats.bytes_allocated = 0;
    }
    
    fn mark_phase(&mut self) {
        // Mark all reachable objects from roots
        for &root in &self.gc_state.roots {
            self.mark_object(root);
        }
    }
    
    fn mark_object(&self, _object_id: ObjectId) {
        // Mark object and recursively mark referenced objects
        // Implementation would depend on object representation
    }
    
    fn sweep_phase(&mut self) {
        // Sweep unmarked objects
        // Implementation would iterate through heap and free unmarked objects
    }
}

// Placeholder types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ContinuationId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjectId(pub u32);

impl ContinuationId {
    pub fn root() -> Self {
        Self(0)
    }
}

#[derive(Debug)]
pub struct CompiledFunction {
    pub code: Vec<u8>,
    pub metadata: FunctionMetadata,
}

#[derive(Debug)]
pub struct FunctionMetadata {
    pub name: String,
    pub parameter_count: usize,
    pub return_type: TypeId,
}

#[derive(Debug)]
pub struct WorkerThread {
    pub id: usize,
    pub stealer: crossbeam::deque::Stealer<WorkItem>,
}

impl WorkerThread {
    pub fn new(id: usize, stealer: crossbeam::deque::Stealer<WorkItem>) -> Self {
        Self { id, stealer }
    }
}

#[derive(Debug)]
pub struct RuntimeStats {
    pub functions_executed: u64,
    pub work_items_processed: u64,
    pub gc_collections: u64,
}

impl RuntimeStats {
    pub fn new() -> Self {
        Self {
            functions_executed: 0,
            work_items_processed: 0,
            gc_collections: 0,
        }
    }
}

#[derive(Debug)]
pub struct AllocationStats {
    pub bytes_allocated: usize,
    pub objects_allocated: usize,
}

impl AllocationStats {
    pub fn new() -> Self {
        Self {
            bytes_allocated: 0,
            objects_allocated: 0,
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeMismatch,
    ArgumentCountMismatch,
    UnknownFunction(FunctionId),
    StackOverflow,
    OutOfMemory,
}
```

## Best Practices

### 1. Choose the Right Tool for the Job

```rust
// ✅ Use bumpalo for temporary allocations in compiler phases
fn parse_with_arena(source: &str) -> Result<CompiledModule, ParseError> {
    let arena = bumpalo::Bump::new();
    let mut parser = ArenaParser::new(&arena, source);
    let ast = parser.parse()?;
    
    // Convert to owned data structure for long-term storage
    let owned_ast = ast.to_owned();
    Ok(CompiledModule { ast: owned_ast })
}

// ✅ Use smallvec for collections that are usually small
type ParameterList = smallvec::SmallVec<[Parameter; 4]>; // Most functions have ≤4 params
type ErrorStack = smallvec::SmallVec<[CompileError; 8]>;  // Error chains usually shallow

// ✅ Use indexmap when order matters
type SymbolTable = indexmap::IndexMap<String, Symbol>; // Declaration order for diagnostics
```

### 2. Integrate Memory Management Strategies

```rust
// ✅ Combine different allocation strategies appropriately
pub struct CompilerPhase {
    // Arena for temporary allocations during this phase
    arena: bumpalo::Bump,
    
    // Persistent data with order preservation
    symbols: indexmap::IndexMap<String, Symbol>,
    
    // Small collections for common cases
    current_scope: smallvec::SmallVec<[ScopeId; 4]>,
    
    // Object pool for frequently allocated types
    string_pool: ObjectPool<String>,
}

impl CompilerPhase {
    pub fn process_module(&mut self, module: &Module) -> Result<ProcessedModule, Error> {
        // Reset arena for new module
        self.arena.reset();
        
        // Use arena for temporary AST nodes
        let ast = self.parse_in_arena(&self.arena, &module.source)?;
        
        // Use persistent storage for symbols
        self.extract_symbols(ast)?;
        
        // Use small collections for local state
        self.current_scope.clear();
        self.current_scope.push(ScopeId::global());
        
        // Convert to owned result
        Ok(ProcessedModule::from_ast(ast))
    }
}
```

### 3. Profile and Measure

```rust
// ✅ Always measure the impact of optimizations
fn benchmark_allocation_strategies() {
    use criterion::{black_box, Criterion};
    
    let mut c = Criterion::default();
    
    // Benchmark different approaches
    c.bench_function("heap_allocation", |b| {
        b.iter(|| {
            let mut nodes = Vec::new();
            for i in 0..1000 {
                nodes.push(Box::new(AstNode::new(black_box(i))));
            }
            nodes
        });
    });
    
    c.bench_function("arena_allocation", |b| {
        b.iter(|| {
            let arena = bumpalo::Bump::new();
            let mut nodes = Vec::new();
            for i in 0..1000 {
                nodes.push(arena.alloc(AstNode::new(black_box(i))));
            }
            nodes
        });
    });
}
```

### 4. Design for Scalability

```rust
// ✅ Design data structures to scale with project size
pub struct ScalableCompiler {
    // Use efficient data structures that scale well
    modules: indexmap::IndexMap<ModuleId, ModuleData>,
    
    // Partition large datasets
    symbol_tables: Vec<indexmap::IndexMap<String, Symbol>>, // One per module
    
    // Use parallel-friendly structures
    work_queue: crossbeam::deque::Injector<CompilationTask>,
    
    // Cache frequently accessed data
    type_cache: std::collections::HashMap<TypeSignature, TypeId>,
}

impl ScalableCompiler {
    pub fn compile_large_project(&mut self, project: &LargeProject) -> Result<CompiledProject, Error> {
        // Process modules in dependency order
        let compilation_order = self.compute_compilation_order(&project.modules)?;
        
        // Use parallel compilation for independent modules
        let results: Vec<_> = compilation_order
            .par_iter()
            .map(|&module_id| self.compile_module(module_id))
            .collect();
        
        // Combine results
        self.link_modules(results)
    }
}
```

### 5. Error Handling and Recovery

```rust
// ✅ Design robust error handling with efficient data structures
pub struct ErrorReporter {
    // Use IndexMap to preserve error order for consistent output
    errors: indexmap::IndexMap<SourceLocation, CompileError>,
    
    // Use SmallVec for error context (usually shallow)
    error_context: smallvec::SmallVec<[String; 4]>,
    
    // Efficient error deduplication
    seen_errors: std::collections::HashSet<ErrorSignature>,
}

impl ErrorReporter {
    pub fn report_error(&mut self, location: SourceLocation, error: CompileError) {
        let signature = ErrorSignature::from(&error);
        
        // Avoid duplicate errors
        if !self.seen_errors.insert(signature) {
            return;
        }
        
        // Store error with location for sorting
        self.errors.insert(location, error);
    }
    
    pub fn emit_errors(&self) -> String {
        let mut output = String::new();
        
        // Emit errors in source order (thanks to IndexMap)
        for (location, error) in &self.errors {
            writeln!(output, "Error at {}: {}", location, error).unwrap();
        }
        
        output
    }
}
```

---

This comprehensive guide demonstrates how to effectively integrate bumpalo, smallvec, and indexmap in compiler implementations, with specific focus on the Parallax compiler architecture. The patterns shown here provide a foundation for building high-performance, memory-efficient compiler infrastructure that scales well with project size and complexity.