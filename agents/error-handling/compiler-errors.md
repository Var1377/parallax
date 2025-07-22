# Compiler Error Handling Best Practices

Compiler error handling requires special consideration due to the need for precise diagnostics, error recovery, and user-friendly messages. This guide covers patterns specifically designed for compiler and language tool development, drawing from the Parallax compiler architecture.

## Table of Contents

1. [Compiler Error Philosophy](#compiler-error-philosophy)
2. [Diagnostic System Design](#diagnostic-system-design)
3. [Error Recovery Patterns](#error-recovery-patterns)
4. [Source Location Tracking](#source-location-tracking)
5. [Error Reporting and Formatting](#error-reporting-and-formatting)
6. [Multi-Phase Error Handling](#multi-phase-error-handling)
7. [Performance Considerations](#performance-considerations)
8. [Testing Compiler Errors](#testing-compiler-errors)

## Compiler Error Philosophy

### Core Principles for Compiler Errors

1. **Precise Location Information**: Every error should point to the exact source location
2. **Helpful Messages**: Explain what went wrong and suggest fixes
3. **Error Recovery**: Continue compilation to find more errors
4. **Consistent Formatting**: Use a standard format for all diagnostics
5. **Actionable Suggestions**: Provide concrete steps to fix the error

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("syntax error at {location}: {message}")]
    Syntax {
        location: SourceLocation,
        message: String,
        help: Option<String>,
    },
    
    #[error("type error: expected {expected}, found {found}")]
    Type {
        expected: String,
        found: String,
        location: SourceLocation,
        note: Option<String>,
    },
    
    #[error("undefined variable: {name}")]
    UndefinedVariable {
        name: String,
        location: SourceLocation,
        suggestion: Option<String>,
    },
}
```

### Error Categories

Organize compiler errors into logical categories:

```rust
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Parse error")]
    Parse(#[from] ParseError),
    
    #[error("Type checking error")]
    TypeCheck(#[from] TypeCheckError),
    
    #[error("Name resolution error")]
    NameResolution(#[from] NameResolutionError),
    
    #[error("Code generation error")]
    CodeGen(#[from] CodeGenError),
    
    #[error("IO error")]
    Io(#[from] std::io::Error),
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
        location: SourceLocation,
    },
    
    #[error("missing semicolon")]
    MissingSemicolon {
        location: SourceLocation,
        suggestion: String,
    },
    
    #[error("unclosed delimiter: {delimiter}")]
    UnclosedDelimiter {
        delimiter: char,
        open_location: SourceLocation,
        expected_close: SourceLocation,
    },
}

#[derive(Error, Debug)]
pub enum TypeCheckError {
    #[error("type mismatch: expected {expected}, found {found}")]
    Mismatch {
        expected: String,
        found: String,
        location: SourceLocation,
        expected_location: Option<SourceLocation>,
    },
    
    #[error("cannot infer type for variable {name}")]
    CannotInfer {
        name: String,
        location: SourceLocation,
        help: String,
    },
    
    #[error("recursive type definition")]
    RecursiveType {
        type_name: String,
        location: SourceLocation,
        cycle: Vec<SourceLocation>,
    },
}
```

## Diagnostic System Design

### Diagnostic Structure

Design a comprehensive diagnostic system:

```rust
use ariadne::{Color, Label, Report, ReportKind, Source};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<DiagnosticLabel>,
    pub notes: Vec<String>,
    pub help: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
    Note,
}

#[derive(Debug, Clone)]
pub struct DiagnosticLabel {
    pub span: SourceSpan,
    pub message: String,
    pub color: Color,
    pub style: LabelStyle,
}

#[derive(Debug, Clone, Copy)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub span: SourceSpan,
}
```

### Error Collection System

Implement a system to collect multiple errors:

```rust
pub struct ErrorCollector {
    diagnostics: Vec<Diagnostic>,
    error_limit: usize,
    has_fatal_error: bool,
}

impl ErrorCollector {
    pub fn new(error_limit: usize) -> Self {
        Self {
            diagnostics: Vec::new(),
            error_limit,
            has_fatal_error: false,
        }
    }
    
    pub fn error(&mut self, diagnostic: Diagnostic) {
        if self.diagnostics.len() < self.error_limit && !self.has_fatal_error {
            self.diagnostics.push(diagnostic);
        }
    }
    
    pub fn fatal_error(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
        self.has_fatal_error = true;
    }
    
    pub fn warning(&mut self, diagnostic: Diagnostic) {
        if self.diagnostics.len() < self.error_limit {
            self.diagnostics.push(diagnostic);
        }
    }
    
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| matches!(d.level, DiagnosticLevel::Error))
    }
    
    pub fn error_count(&self) -> usize {
        self.diagnostics.iter()
            .filter(|d| matches!(d.level, DiagnosticLevel::Error))
            .count()
    }
    
    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}
```

### Diagnostic Builder Pattern

Create a builder for constructing diagnostics:

```rust
pub struct DiagnosticBuilder {
    level: DiagnosticLevel,
    code: Option<String>,
    message: String,
    labels: Vec<DiagnosticLabel>,
    notes: Vec<String>,
    help: Option<String>,
}

impl DiagnosticBuilder {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            code: None,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
            help: None,
        }
    }
    
    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            level: DiagnosticLevel::Warning,
            code: None,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
            help: None,
        }
    }
    
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }
    
    pub fn with_label(mut self, span: SourceSpan, message: impl Into<String>) -> Self {
        self.labels.push(DiagnosticLabel {
            span,
            message: message.into(),
            color: Color::Red,
            style: LabelStyle::Primary,
        });
        self
    }
    
    pub fn with_secondary_label(mut self, span: SourceSpan, message: impl Into<String>) -> Self {
        self.labels.push(DiagnosticLabel {
            span,
            message: message.into(),
            color: Color::Blue,
            style: LabelStyle::Secondary,
        });
        self
    }
    
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
    
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
    
    pub fn build(self) -> Diagnostic {
        Diagnostic {
            level: self.level,
            code: self.code,
            message: self.message,
            labels: self.labels,
            notes: self.notes,
            help: self.help,
        }
    }
}

// Usage example
fn create_type_error(expected: &str, found: &str, location: SourceSpan) -> Diagnostic {
    DiagnosticBuilder::error(format!("type mismatch: expected {}, found {}", expected, found))
        .with_code("E0308")
        .with_label(location, format!("expected {}", expected))
        .with_help("try converting the value to the expected type")
        .build()
}
```

## Error Recovery Patterns

### Parser Error Recovery

Implement robust error recovery in parsers:

```rust
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: ErrorCollector,
}

impl Parser {
    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.parse_statement_inner() {
            Ok(stmt) => Some(stmt),
            Err(error) => {
                self.errors.error(error);
                self.recover_to_statement_boundary();
                None
            }
        }
    }
    
    fn parse_statement_inner(&mut self) -> Result<Statement, Diagnostic> {
        match self.current_token() {
            Token::Let => self.parse_let_statement(),
            Token::If => self.parse_if_statement(),
            Token::While => self.parse_while_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    
    fn recover_to_statement_boundary(&mut self) {
        // Skip tokens until we find a statement boundary
        while !self.is_at_end() {
            match self.current_token() {
                Token::Semicolon => {
                    self.advance(); // Consume the semicolon
                    break;
                }
                Token::Let | Token::If | Token::While | Token::LeftBrace => {
                    // Found start of next statement
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }
    
    fn parse_expression_statement(&mut self) -> Result<Statement, Diagnostic> {
        let expr = self.parse_expression()?;
        
        if !self.check(&Token::Semicolon) {
            // Try to recover by inserting a semicolon
            let diagnostic = DiagnosticBuilder::error("expected ';' after expression")
                .with_code("E0001")
                .with_label(self.previous_span(), "expected ';' here")
                .with_help("add a semicolon to terminate the statement")
                .build();
            
            self.errors.error(diagnostic);
            
            // Continue parsing as if semicolon was present
            return Ok(Statement::Expression(expr));
        }
        
        self.advance(); // consume semicolon
        Ok(Statement::Expression(expr))
    }
}
```

### Type Checker Error Recovery

Implement error recovery in type checking:

```rust
pub struct TypeChecker {
    errors: ErrorCollector,
    type_env: TypeEnvironment,
}

impl TypeChecker {
    pub fn check_expression(&mut self, expr: &Expression) -> Type {
        match self.check_expression_inner(expr) {
            Ok(ty) => ty,
            Err(diagnostic) => {
                self.errors.error(diagnostic);
                // Return error type to continue checking
                Type::Error
            }
        }
    }
    
    fn check_expression_inner(&mut self, expr: &Expression) -> Result<Type, Diagnostic> {
        match expr {
            Expression::Variable { name, span } => {
                match self.type_env.lookup(name) {
                    Some(ty) => Ok(ty.clone()),
                    None => {
                        let suggestion = self.suggest_similar_variable(name);
                        let mut builder = DiagnosticBuilder::error(format!("undefined variable: {}", name))
                            .with_code("E0425")
                            .with_label(*span, "not found in this scope");
                        
                        if let Some(suggestion) = suggestion {
                            builder = builder.with_help(format!("did you mean '{}'?", suggestion));
                        }
                        
                        Err(builder.build())
                    }
                }
            }
            
            Expression::BinaryOp { left, op, right, span } => {
                let left_ty = self.check_expression(left);
                let right_ty = self.check_expression(right);
                
                // Continue checking even if operands have errors
                if matches!(left_ty, Type::Error) || matches!(right_ty, Type::Error) {
                    return Ok(Type::Error);
                }
                
                self.check_binary_op_types(&left_ty, op, &right_ty, *span)
            }
            
            Expression::FunctionCall { name, args, span } => {
                let function_ty = self.check_expression(name);
                let arg_types: Vec<Type> = args.iter()
                    .map(|arg| self.check_expression(arg))
                    .collect();
                
                self.check_function_call(&function_ty, &arg_types, *span)
            }
        }
    }
    
    fn suggest_similar_variable(&self, name: &str) -> Option<String> {
        let variables: Vec<&String> = self.type_env.variables().collect();
        
        // Simple edit distance suggestion
        variables.into_iter()
            .filter(|var| edit_distance(name, var) <= 2)
            .min_by_key(|var| edit_distance(name, var))
            .cloned()
    }
}
```

## Source Location Tracking

### Span Tracking

Implement comprehensive span tracking:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
    
    pub fn contains(self, pos: usize) -> bool {
        pos >= self.start && pos < self.end
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
    
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }
}
```

### Source Map

Maintain a mapping from byte positions to line/column:

```rust
#[derive(Debug)]
pub struct SourceMap {
    source: String,
    line_starts: Vec<usize>,
    file_name: String,
}

impl SourceMap {
    pub fn new(source: String, file_name: String) -> Self {
        let mut line_starts = vec![0];
        
        for (i, ch) in source.char_indices() {
            if ch == '\n' {
                line_starts.push(i + 1);
            }
        }
        
        Self {
            source,
            line_starts,
            file_name,
        }
    }
    
    pub fn location(&self, pos: usize) -> SourceLocation {
        let line = self.line_starts
            .binary_search(&pos)
            .unwrap_or_else(|i| i - 1);
        
        let line_start = self.line_starts[line];
        let column = pos - line_start;
        
        SourceLocation {
            file: self.file_name.clone(),
            line: line + 1, // 1-based
            column: column + 1, // 1-based
            span: SourceSpan { start: pos, end: pos + 1 },
        }
    }
    
    pub fn span_to_location(&self, span: Span) -> (SourceLocation, SourceLocation) {
        (self.location(span.start), self.location(span.end))
    }
    
    pub fn get_line(&self, line_number: usize) -> Option<&str> {
        if line_number == 0 || line_number > self.line_starts.len() {
            return None;
        }
        
        let start = self.line_starts[line_number - 1];
        let end = if line_number < self.line_starts.len() {
            self.line_starts[line_number] - 1 // Exclude newline
        } else {
            self.source.len()
        };
        
        Some(&self.source[start..end])
    }
}
```

## Error Reporting and Formatting

### Rich Error Display

Implement rich error formatting using `ariadne`:

```rust
use ariadne::{Color, Label, Report, ReportKind, Source};

pub fn render_diagnostic(diagnostic: &Diagnostic, source_map: &SourceMap) -> String {
    let mut report = Report::build(
        match diagnostic.level {
            DiagnosticLevel::Error => ReportKind::Error,
            DiagnosticLevel::Warning => ReportKind::Warning,
            DiagnosticLevel::Info => ReportKind::Advice,
            DiagnosticLevel::Note => ReportKind::Advice,
        },
        &source_map.file_name,
        diagnostic.labels.first().map(|l| l.span.start).unwrap_or(0),
    )
    .with_message(&diagnostic.message);
    
    if let Some(code) = &diagnostic.code {
        report = report.with_code(code);
    }
    
    for label in &diagnostic.labels {
        let label_color = match diagnostic.level {
            DiagnosticLevel::Error => Color::Red,
            DiagnosticLevel::Warning => Color::Yellow,
            _ => Color::Blue,
        };
        
        report = report.with_label(
            Label::new((&source_map.file_name, label.span.start..label.span.end))
                .with_message(&label.message)
                .with_color(label_color)
        );
    }
    
    for note in &diagnostic.notes {
        report = report.with_note(note);
    }
    
    if let Some(help) = &diagnostic.help {
        report = report.with_help(help);
    }
    
    let mut output = Vec::new();
    report.finish()
        .write((&source_map.file_name, Source::from(&source_map.source)), &mut output)
        .unwrap();
    
    String::from_utf8(output).unwrap()
}
```

### Error Message Guidelines

Follow consistent guidelines for error messages:

```rust
pub struct ErrorMessageGuidelines;

impl ErrorMessageGuidelines {
    // Good: Specific and actionable
    pub fn good_type_error() -> Diagnostic {
        DiagnosticBuilder::error("mismatched types")
            .with_code("E0308")
            .with_label(Span::new(10, 15), "expected `i32`, found `&str`")
            .with_help("try parsing the string: `\"hello\".parse::<i32>()?`")
            .build()
    }
    
    // Good: Clear explanation with context
    pub fn good_undefined_variable() -> Diagnostic {
        DiagnosticBuilder::error("cannot find value `foo` in this scope")
            .with_code("E0425")
            .with_label(Span::new(20, 23), "not found in this scope")
            .with_help("did you mean `bar`?")
            .build()
    }
    
    // Avoid: Vague and unhelpful
    pub fn bad_error() -> Diagnostic {
        DiagnosticBuilder::error("syntax error")
            .with_label(Span::new(10, 15), "here")
            .build()
    }
}
```

## Multi-Phase Error Handling

### Compilation Pipeline

Structure error handling across compilation phases:

```rust
pub struct CompilationResult<T> {
    pub result: Option<T>,
    pub diagnostics: Vec<Diagnostic>,
}

impl<T> CompilationResult<T> {
    pub fn success(value: T) -> Self {
        Self {
            result: Some(value),
            diagnostics: Vec::new(),
        }
    }
    
    pub fn failure(diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            result: None,
            diagnostics,
        }
    }
    
    pub fn with_warnings(value: T, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            result: Some(value),
            diagnostics,
        }
    }
    
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| matches!(d.level, DiagnosticLevel::Error))
    }
}

pub struct Compiler {
    source_map: SourceMap,
}

impl Compiler {
    pub fn compile(&mut self, source: &str) -> CompilationResult<CompiledProgram> {
        let mut all_diagnostics = Vec::new();
        
        // Phase 1: Lexing
        let tokens = match self.lex(source) {
            CompilationResult { result: Some(tokens), mut diagnostics } => {
                all_diagnostics.append(&mut diagnostics);
                tokens
            }
            CompilationResult { result: None, mut diagnostics } => {
                all_diagnostics.append(&mut diagnostics);
                return CompilationResult::failure(all_diagnostics);
            }
        };
        
        // Phase 2: Parsing
        let ast = match self.parse(tokens) {
            CompilationResult { result: Some(ast), mut diagnostics } => {
                all_diagnostics.append(&mut diagnostics);
                ast
            }
            CompilationResult { result: None, mut diagnostics } => {
                all_diagnostics.append(&mut diagnostics);
                return CompilationResult::failure(all_diagnostics);
            }
        };
        
        // Phase 3: Type Checking
        let typed_ast = match self.type_check(ast) {
            CompilationResult { result: Some(typed_ast), mut diagnostics } => {
                all_diagnostics.append(&mut diagnostics);
                typed_ast
            }
            CompilationResult { result: None, mut diagnostics } => {
                all_diagnostics.append(&mut diagnostics);
                return CompilationResult::failure(all_diagnostics);
            }
        };
        
        // Phase 4: Code Generation
        match self.generate_code(typed_ast) {
            CompilationResult { result: Some(program), mut diagnostics } => {
                all_diagnostics.append(&mut diagnostics);
                CompilationResult::with_warnings(program, all_diagnostics)
            }
            CompilationResult { result: None, mut diagnostics } => {
                all_diagnostics.append(&mut diagnostics);
                CompilationResult::failure(all_diagnostics)
            }
        }
    }
}
```

### Error Limits and Cascading

Prevent error cascading and implement limits:

```rust
pub struct CompilerConfig {
    pub max_errors: usize,
    pub max_warnings: usize,
    pub stop_on_first_error: bool,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            max_errors: 100,
            max_warnings: 1000,
            stop_on_first_error: false,
        }
    }
}

pub struct PhaseErrorCollector {
    config: CompilerConfig,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
    has_fatal_error: bool,
}

impl PhaseErrorCollector {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            config,
            errors: Vec::new(),
            warnings: Vec::new(),
            has_fatal_error: false,
        }
    }
    
    pub fn error(&mut self, diagnostic: Diagnostic) -> bool {
        if self.errors.len() >= self.config.max_errors {
            return false; // Stop collecting errors
        }
        
        self.errors.push(diagnostic);
        
        if self.config.stop_on_first_error {
            self.has_fatal_error = true;
        }
        
        true
    }
    
    pub fn warning(&mut self, diagnostic: Diagnostic) {
        if self.warnings.len() < self.config.max_warnings {
            self.warnings.push(diagnostic);
        }
    }
    
    pub fn should_continue(&self) -> bool {
        !self.has_fatal_error && self.errors.len() < self.config.max_errors
    }
    
    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        let mut diagnostics = self.errors;
        diagnostics.extend(self.warnings);
        diagnostics
    }
}
```

## Performance Considerations

### Efficient Error Collection

Optimize error collection for performance:

```rust
use std::sync::Arc;

// Use Arc for shared error data
#[derive(Debug, Clone)]
pub struct SharedDiagnostic {
    pub level: DiagnosticLevel,
    pub code: Option<Arc<str>>,
    pub message: Arc<str>,
    pub labels: Vec<DiagnosticLabel>,
    pub notes: Vec<Arc<str>>,
    pub help: Option<Arc<str>>,
}

// Pool common error messages
pub struct ErrorMessagePool {
    common_messages: std::collections::HashMap<&'static str, Arc<str>>,
}

impl ErrorMessagePool {
    pub fn new() -> Self {
        let mut pool = Self {
            common_messages: std::collections::HashMap::new(),
        };
        
        // Pre-allocate common messages
        pool.add_message("expected ';'");
        pool.add_message("unexpected token");
        pool.add_message("type mismatch");
        pool.add_message("undefined variable");
        
        pool
    }
    
    fn add_message(&mut self, message: &'static str) {
        self.common_messages.insert(message, Arc::from(message));
    }
    
    pub fn get_or_create(&mut self, message: &str) -> Arc<str> {
        if let Some(pooled) = self.common_messages.get(message) {
            pooled.clone()
        } else {
            Arc::from(message)
        }
    }
}
```

### Lazy Error Formatting

Defer expensive error formatting until needed:

```rust
pub struct LazyDiagnostic {
    level: DiagnosticLevel,
    code: Option<String>,
    message_fn: Box<dyn Fn() -> String + Send + Sync>,
    span: Span,
}

impl LazyDiagnostic {
    pub fn new<F>(level: DiagnosticLevel, span: Span, message_fn: F) -> Self
    where
        F: Fn() -> String + Send + Sync + 'static,
    {
        Self {
            level,
            code: None,
            message_fn: Box::new(message_fn),
            span,
        }
    }
    
    pub fn materialize(self) -> Diagnostic {
        Diagnostic {
            level: self.level,
            code: self.code,
            message: (self.message_fn)(),
            labels: vec![DiagnosticLabel {
                span: SourceSpan { start: self.span.start, end: self.span.end },
                message: String::new(),
                color: Color::Red,
                style: LabelStyle::Primary,
            }],
            notes: Vec::new(),
            help: None,
        }
    }
}
```

## Testing Compiler Errors

### Snapshot Testing for Error Messages

Use snapshot testing to ensure error message stability:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    
    fn check_error(source: &str, expected: Expect) {
        let mut compiler = Compiler::new();
        let result = compiler.compile(source);
        
        let error_output = result.diagnostics
            .iter()
            .map(|d| render_diagnostic(d, &compiler.source_map))
            .collect::<Vec<_>>()
            .join("\n");
        
        expected.assert_eq(&error_output);
    }
    
    #[test]
    fn test_type_error_message() {
        check_error(
            r#"let x: i32 = "hello";"#,
            expect![[r#"
                error[E0308]: mismatched types
                  --> test.px:1:14
                   |
                 1 | let x: i32 = "hello";
                   |        ---   ^^^^^^^ expected `i32`, found `&str`
                   |        |
                   |        expected due to this type annotation
                   |
                   = help: try parsing the string: `"hello".parse::<i32>()?`
            "#]],
        );
    }
    
    #[test]
    fn test_undefined_variable_with_suggestion() {
        check_error(
            r#"
            let hello = 42;
            println!(helo);
            "#,
            expect![[r#"
                error[E0425]: cannot find value `helo` in this scope
                  --> test.px:3:22
                   |
                 3 |             println!(helo);
                   |                      ^^^^ not found in this scope
                   |
                   = help: did you mean `hello`?
            "#]],
        );
    }
}
```

### Property-Based Testing

Test error handling properties:

```rust
#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;
    
    proptest! {
        #[test]
        fn parser_never_panics(source in ".*") {
            let mut parser = Parser::new(&source);
            let _ = parser.parse(); // Should never panic
        }
        
        #[test]
        fn all_errors_have_valid_spans(source in ".*") {
            let mut compiler = Compiler::new();
            let result = compiler.compile(&source);
            
            for diagnostic in result.diagnostics {
                for label in diagnostic.labels {
                    prop_assert!(label.span.start <= source.len());
                    prop_assert!(label.span.end <= source.len());
                    prop_assert!(label.span.start <= label.span.end);
                }
            }
        }
        
        #[test]
        fn error_recovery_finds_multiple_errors(
            statements in prop::collection::vec("let [a-z]+ = [0-9]+", 1..10)
        ) {
            let source = statements.join(";\n"); // Missing semicolons
            let mut compiler = Compiler::new();
            let result = compiler.compile(&source);
            
            // Should find multiple syntax errors, not just the first one
            let error_count = result.diagnostics.iter()
                .filter(|d| matches!(d.level, DiagnosticLevel::Error))
                .count();
            
            if statements.len() > 1 {
                prop_assert!(error_count > 1);
            }
        }
    }
}
```

---

*Compiler error handling is about creating a great developer experience. Good error messages can turn a frustrating debugging session into a learning opportunity.*