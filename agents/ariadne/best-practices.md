# Ariadne Best Practices Guide

This guide covers best practices for using Ariadne effectively in production environments, focusing on performance, maintainability, and user experience.

## Table of Contents

1. [Error Message Design](#error-message-design)
2. [Performance Optimization](#performance-optimization)
3. [Code Organization](#code-organization)
4. [Testing Strategies](#testing-strategies)
5. [Accessibility and Internationalization](#accessibility-and-internationalization)
6. [Integration Patterns](#integration-patterns)
7. [Common Pitfalls](#common-pitfalls)

## Error Message Design

### Write for Your Users

**Do:**
```rust
Report::build(ReportKind::Error, span)
    .with_message("Cannot assign to immutable variable")
    .with_label(
        Label::new(var_span)
            .with_message("This variable is not mutable")
            .with_color(Color::Red)
    )
    .with_help("Add `mut` to make it mutable: `let mut x = ...`")
    .finish()
```

**Don't:**
```rust
Report::build(ReportKind::Error, span)
    .with_message("E0384: cannot assign twice to immutable variable")
    .finish()
```

### Provide Context and Solutions

**Good Error Messages:**
- Explain what went wrong in plain language
- Show the exact location of the problem
- Suggest concrete solutions
- Include relevant context

```rust
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind};

fn good_error_example() {
    let mut colors = ColorGenerator::new();
    let expected_color = colors.next();
    let found_color = colors.next();
    
    Report::build(ReportKind::Error, function_call_span)
        .with_code("TYPE_MISMATCH")
        .with_message("Function argument has wrong type")
        .with_label(
            Label::new(function_def_span)
                .with_message(format!("Function expects {}", "i32".fg(expected_color)))
                .with_color(expected_color)
        )
        .with_label(
            Label::new(argument_span)
                .with_message(format!("Argument has type {}", "String".fg(found_color)))
                .with_color(found_color)
        )
        .with_note(format!(
            "Cannot convert {} to {} automatically",
            "String".fg(found_color),
            "i32".fg(expected_color)
        ))
        .with_help("Try parsing the string: `arg.parse::<i32>()?`")
        .with_help("Or change the function to accept String")
        .finish()
}
```

### Use Consistent Color Schemes

```rust
use ariadne::{Color, ColorGenerator};

struct DiagnosticColors {
    error: Color,
    warning: Color,
    info: Color,
    type_name: Color,
    variable: Color,
    keyword: Color,
}

impl DiagnosticColors {
    fn new() -> Self {
        let mut gen = ColorGenerator::new();
        Self {
            error: Color::Red,
            warning: Color::Yellow,
            info: Color::Blue,
            type_name: gen.next(),
            variable: gen.next(),
            keyword: gen.next(),
        }
    }
}

// Use consistent colors throughout your application
fn consistent_coloring(colors: &DiagnosticColors) {
    Report::build(ReportKind::Error, span)
        .with_label(
            Label::new(type_span)
                .with_message(format!("Type {}", "i32".fg(colors.type_name)))
                .with_color(colors.type_name)
        )
        .with_label(
            Label::new(var_span)
                .with_message(format!("Variable {}", "x".fg(colors.variable)))
                .with_color(colors.variable)
        )
        .finish()
}
```

### Progressive Disclosure

Start with the most important information, then provide details:

```rust
fn progressive_disclosure_example() {
    Report::build(ReportKind::Error, span)
        // Primary message - what went wrong
        .with_message("Type mismatch in assignment")
        
        // Primary labels - show the conflict
        .with_label(
            Label::new(lhs_span)
                .with_message("Expected i32")
                .with_color(Color::Blue)
                .with_priority(100)
        )
        .with_label(
            Label::new(rhs_span)
                .with_message("Found String")
                .with_color(Color::Red)
                .with_priority(100)
        )
        
        // Secondary context
        .with_label(
            Label::new(declaration_span)
                .with_message("Variable declared here")
                .with_color(Color::Cyan)
                .with_priority(50)
        )
        
        // Explanation
        .with_note("Rust requires explicit type conversions")
        
        // Solutions (most common first)
        .with_help("Convert to integer: `value.parse::<i32>()?`")
        .with_help("Or change variable type to String")
        .finish()
}
```

## Performance Optimization

### Source Management

**Use appropriate cache types:**

```rust
use ariadne::{FileCache, FnCache, sources};
use std::sync::Arc;

// For file-based projects
fn file_based_cache() -> FileCache {
    FileCache::default() // Automatically manages file loading
}

// For in-memory content with sharing
fn shared_content_cache() -> impl Cache<String> {
    let content: Arc<str> = Arc::from("shared content");
    sources(vec![
        ("file1".to_string(), content.clone()),
        ("file2".to_string(), content.clone()),
    ])
}

// For custom loading logic
fn custom_cache() -> impl Cache<String> {
    FnCache::new(|path: &String| {
        // Custom loading logic - database, network, etc.
        load_from_custom_source(path)
    })
}
```

**Optimize for your use case:**

```rust
use ariadne::{Config, IndexType, CharSet};

// Memory-constrained environments
fn memory_optimized_config() -> Config {
    Config::default()
        .with_compact(true)
        .with_index_type(IndexType::Byte) // O(1) lookups
        .with_char_set(CharSet::Ascii)
        .with_color(false)
}

// Performance-critical paths
fn performance_config() -> Config {
    Config::default()
        .with_index_type(IndexType::Byte)
        .with_multiline_arrows(false)
        .with_underlines(false)
}
```

### Batch Processing

```rust
use ariadne::{Report, ReportKind, Label, Color};

struct ErrorBatch<S> {
    reports: Vec<Report<S>>,
    max_errors: usize,
    error_count: usize,
}

impl<S: Span> ErrorBatch<S> {
    fn new(max_errors: usize) -> Self {
        Self {
            reports: Vec::with_capacity(max_errors.min(100)),
            max_errors,
            error_count: 0,
        }
    }
    
    fn add_error(&mut self, report: Report<S>) -> bool {
        self.error_count += 1;
        
        if self.reports.len() < self.max_errors {
            self.reports.push(report);
            true
        } else {
            false // Silently drop additional errors
        }
    }
    
    fn flush<C: Cache<S::SourceId>>(&mut self, cache: C) -> std::io::Result<()> {
        // Sort by severity and location
        self.reports.sort_by(|a, b| {
            a.kind.cmp(&b.kind)
                .then_with(|| a.span.start().cmp(&b.span.start()))
        });
        
        for report in &self.reports {
            report.print(&cache)?;
        }
        
        if self.error_count > self.max_errors {
            eprintln!("... and {} more errors", self.error_count - self.max_errors);
        }
        
        self.reports.clear();
        Ok(())
    }
}
```

### Streaming for Large Outputs

```rust
use ariadne::{Report, Source};
use std::io::{Write, BufWriter};

struct StreamingDiagnostics<W: Write> {
    writer: BufWriter<W>,
    config: Config,
}

impl<W: Write> StreamingDiagnostics<W> {
    fn new(writer: W) -> Self {
        Self {
            writer: BufWriter::new(writer),
            config: Config::default(),
        }
    }
    
    fn emit<S: Span, C: Cache<S::SourceId>>(&mut self, report: Report<S>, cache: C) -> std::io::Result<()> {
        report.write_with_config(cache, &mut self.writer, &self.config)?;
        self.writer.flush()?;
        Ok(())
    }
}
```

## Code Organization

### Structured Error Types

```rust
use ariadne::{Color, Label, Report, ReportKind};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Syntax error: {message}")]
    Syntax {
        message: String,
        span: Span,
        help: Option<String>,
    },
    
    #[error("Type error: {message}")]
    Type {
        message: String,
        primary_span: Span,
        secondary_spans: Vec<(Span, String)>,
        note: Option<String>,
    },
    
    #[error("Name resolution error: {name} not found")]
    NameResolution {
        name: String,
        span: Span,
        suggestions: Vec<String>,
    },
}

impl CompilerError {
    fn to_report(&self) -> Report<Span> {
        match self {
            CompilerError::Syntax { message, span, help } => {
                let mut builder = Report::build(ReportKind::Error, span.clone())
                    .with_code("SYNTAX")
                    .with_message(message)
                    .with_label(
                        Label::new(span.clone())
                            .with_message("Syntax error here")
                            .with_color(Color::Red)
                    );
                
                if let Some(help_text) = help {
                    builder = builder.with_help(help_text);
                }
                
                builder.finish()
            }
            
            CompilerError::Type { message, primary_span, secondary_spans, note } => {
                let mut builder = Report::build(ReportKind::Error, primary_span.clone())
                    .with_code("TYPE")
                    .with_message(message)
                    .with_label(
                        Label::new(primary_span.clone())
                            .with_message("Type error here")
                            .with_color(Color::Red)
                    );
                
                for (span, msg) in secondary_spans {
                    builder = builder.with_label(
                        Label::new(span.clone())
                            .with_message(msg)
                            .with_color(Color::Blue)
                    );
                }
                
                if let Some(note_text) = note {
                    builder = builder.with_note(note_text);
                }
                
                builder.finish()
            }
            
            CompilerError::NameResolution { name, span, suggestions } => {
                let mut builder = Report::build(ReportKind::Error, span.clone())
                    .with_code("NAME_NOT_FOUND")
                    .with_message(format!("Cannot find `{}` in this scope", name))
                    .with_label(
                        Label::new(span.clone())
                            .with_message("Not found")
                            .with_color(Color::Red)
                    );
                
                if !suggestions.is_empty() {
                    builder = builder.with_help("Did you mean:");
                    for suggestion in suggestions {
                        builder = builder.with_help(format!("  {}", suggestion));
                    }
                }
                
                builder.finish()
            }
        }
    }
}
```

### Diagnostic Builder Pattern

```rust
use ariadne::{Color, ColorGenerator, Label, Report, ReportKind};

pub struct DiagnosticBuilder {
    colors: ColorGenerator,
    config: Config,
}

impl DiagnosticBuilder {
    pub fn new() -> Self {
        Self {
            colors: ColorGenerator::new(),
            config: Config::default(),
        }
    }
    
    pub fn type_mismatch(&mut self, expected_span: Span, found_span: Span, expected: &str, found: &str) -> Report<Span> {
        let expected_color = self.colors.next();
        let found_color = self.colors.next();
        
        Report::build(ReportKind::Error, found_span.clone())
            .with_code("TYPE_MISMATCH")
            .with_message(format!("Expected {}, found {}", expected, found))
            .with_label(
                Label::new(expected_span)
                    .with_message(format!("Expected {}", expected.fg(expected_color)))
                    .with_color(expected_color)
            )
            .with_label(
                Label::new(found_span)
                    .with_message(format!("Found {}", found.fg(found_color)))
                    .with_color(found_color)
            )
            .finish()
    }
    
    pub fn undefined_variable(&mut self, span: Span, name: &str, suggestions: &[&str]) -> Report<Span> {
        let mut builder = Report::build(ReportKind::Error, span.clone())
            .with_code("UNDEFINED_VAR")
            .with_message(format!("Undefined variable `{}`", name))
            .with_label(
                Label::new(span)
                    .with_message("Not defined")
                    .with_color(Color::Red)
            );
        
        if !suggestions.is_empty() {
            builder = builder.with_help("Did you mean:");
            for suggestion in suggestions {
                builder = builder.with_help(format!("  {}", suggestion));
            }
        }
        
        builder.finish()
    }
}
```

## Testing Strategies

### Snapshot Testing

```rust
use ariadne::{Report, ReportKind, Label, Color, Source};

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;
    
    fn render_report_to_string(report: Report<std::ops::Range<usize>>, source: &str) -> String {
        let mut buffer = Vec::new();
        report.write(Source::from(source), &mut buffer).unwrap();
        String::from_utf8(buffer).unwrap()
    }
    
    #[test]
    fn test_type_error_output() {
        let source = "let x: i32 = \"hello\";";
        let report = Report::build(ReportKind::Error, 13..20)
            .with_message("Type mismatch")
            .with_label(
                Label::new(7..10)
                    .with_message("Expected i32")
                    .with_color(Color::Blue)
            )
            .with_label(
                Label::new(13..20)
                    .with_message("Found &str")
                    .with_color(Color::Red)
            )
            .finish();
        
        let output = render_report_to_string(report, source);
        assert_snapshot!(output);
    }
}
```

### Property-Based Testing

```rust
use ariadne::{Report, ReportKind, Label, Source};
use proptest::prelude::*;

proptest! {
    #[test]
    fn report_never_panics(
        start in 0usize..1000,
        end in 0usize..1000,
        message in ".*",
        source_text in ".*"
    ) {
        let start = start.min(source_text.len());
        let end = end.max(start).min(source_text.len());
        
        let report = Report::build(ReportKind::Error, start..end)
            .with_message(message)
            .with_label(
                Label::new(start..end)
                    .with_message("Test label")
            )
            .finish();
        
        let source = Source::from(source_text);
        let mut buffer = Vec::new();
        
        // Should never panic
        let _ = report.write(&source, &mut buffer);
    }
}
```

### Integration Testing

```rust
use ariadne::{FileCache, Report, ReportKind};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_multi_file_diagnostics() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create test files
    fs::write(
        temp_dir.path().join("main.rs"),
        "mod utils;\nfn main() { utils::broken(); }"
    ).unwrap();
    
    fs::write(
        temp_dir.path().join("utils.rs"),
        "pub fn broken() { undefined_var; }"
    ).unwrap();
    
    let mut cache = FileCache::default();
    
    let report = Report::build(ReportKind::Error, (temp_dir.path().join("utils.rs"), 18..31))
        .with_message("Undefined variable")
        .finish();
    
    // Test that multi-file reporting works
    let mut buffer = Vec::new();
    report.write(&mut cache, &mut buffer).unwrap();
    
    let output = String::from_utf8(buffer).unwrap();
    assert!(output.contains("utils.rs"));
    assert!(output.contains("undefined_var"));
}
```

## Accessibility and Internationalization

### Screen Reader Support

```rust
use ariadne::{Config, CharSet};

fn accessible_config() -> Config {
    Config::default()
        .with_color(false)           // No colors for screen readers
        .with_char_set(CharSet::Ascii) // ASCII-only characters
        .with_compact(true)          // Reduce verbosity
}

// Provide text-only alternative
fn text_only_report(report: &Report<Span>) -> String {
    format!(
        "{}: {} at line {}, column {}",
        report.kind,
        report.msg.as_ref().unwrap_or(&"Error".to_string()),
        // Calculate line/column from span
        1, 1
    )
}
```

### Internationalization Support

```rust
use ariadne::{Report, ReportKind, Label};

struct I18nMessages {
    locale: String,
}

impl I18nMessages {
    fn new(locale: String) -> Self {
        Self { locale }
    }
    
    fn type_mismatch(&self, expected: &str, found: &str) -> String {
        match self.locale.as_str() {
            "en" => format!("Expected {}, found {}", expected, found),
            "es" => format!("Se esperaba {}, se encontró {}", expected, found),
            "fr" => format!("Attendu {}, trouvé {}", expected, found),
            _ => format!("Expected {}, found {}", expected, found), // fallback
        }
    }
    
    fn undefined_variable(&self, name: &str) -> String {
        match self.locale.as_str() {
            "en" => format!("Undefined variable `{}`", name),
            "es" => format!("Variable indefinida `{}`", name),
            "fr" => format!("Variable non définie `{}`", name),
            _ => format!("Undefined variable `{}`", name),
        }
    }
}

fn localized_report(messages: &I18nMessages, span: Span, expected: &str, found: &str) -> Report<Span> {
    Report::build(ReportKind::Error, span.clone())
        .with_message(messages.type_mismatch(expected, found))
        .with_label(
            Label::new(span)
                .with_message(messages.type_mismatch(expected, found))
        )
        .finish()
}
```

## Integration Patterns

### Language Server Protocol

```rust
use ariadne::{Report, ReportKind, Label, Color};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

fn ariadne_to_lsp_diagnostic(
    report: &Report<(String, std::ops::Range<usize>)>,
    source: &Source
) -> Diagnostic {
    let severity = match report.kind {
        ReportKind::Error => Some(DiagnosticSeverity::ERROR),
        ReportKind::Warning => Some(DiagnosticSeverity::WARNING),
        ReportKind::Advice => Some(DiagnosticSeverity::HINT),
        ReportKind::Custom(_, _) => Some(DiagnosticSeverity::INFORMATION),
    };
    
    let range = offset_range_to_lsp_range(&report.span.1, source);
    
    Diagnostic {
        range,
        severity,
        code: report.code.clone().map(lsp_types::NumberOrString::String),
        source: Some("your-language".to_string()),
        message: report.msg.clone().unwrap_or_default(),
        related_information: Some(
            report.labels.iter()
                .map(|label| lsp_types::DiagnosticRelatedInformation {
                    location: lsp_types::Location {
                        uri: lsp_types::Url::from_file_path(&label.span.0).unwrap(),
                        range: offset_range_to_lsp_range(&label.span.1, source),
                    },
                    message: label.display_info.msg.clone().unwrap_or_default(),
                })
                .collect()
        ),
        ..Default::default()
    }
}

fn offset_range_to_lsp_range(range: &std::ops::Range<usize>, source: &Source) -> Range {
    let start_pos = offset_to_position(range.start, source);
    let end_pos = offset_to_position(range.end, source);
    Range::new(start_pos, end_pos)
}

fn offset_to_position(offset: usize, source: &Source) -> Position {
    if let Some((line, line_idx, col)) = source.get_offset_line(offset) {
        Position::new(line_idx as u32, col as u32)
    } else {
        Position::new(0, 0)
    }
}
```

### Build System Integration

```rust
use ariadne::{Report, ReportKind, FileCache};
use std::process::ExitCode;

struct BuildDiagnostics {
    cache: FileCache,
    error_count: usize,
    warning_count: usize,
}

impl BuildDiagnostics {
    fn new() -> Self {
        Self {
            cache: FileCache::default(),
            error_count: 0,
            warning_count: 0,
        }
    }
    
    fn emit(&mut self, report: Report<std::path::PathBuf>) -> std::io::Result<()> {
        match report.kind {
            ReportKind::Error => self.error_count += 1,
            ReportKind::Warning => self.warning_count += 1,
            _ => {}
        }
        
        report.eprint(&mut self.cache)
    }
    
    fn finish(self) -> ExitCode {
        eprintln!("Build finished with {} errors, {} warnings", 
                 self.error_count, self.warning_count);
        
        if self.error_count > 0 {
            ExitCode::FAILURE
        } else {
            ExitCode::SUCCESS
        }
    }
}
```

## Common Pitfalls

### Avoid These Mistakes

**1. Creating spans that exceed source bounds:**
```rust
// BAD: Can cause panics
let span = 0..source.len() + 10;

// GOOD: Validate spans
let span = 0..source.len().min(end_offset);
```

**2. Using too many colors:**
```rust
// BAD: Rainbow vomit
for (i, label) in labels.iter().enumerate() {
    label.with_color(Color::Fixed(i as u8 % 255))
}

// GOOD: Consistent color scheme
let mut colors = ColorGenerator::new();
let error_color = colors.next();
let context_color = colors.next();
```

**3. Overwhelming users with information:**
```rust
// BAD: Too much information
Report::build(ReportKind::Error, span)
    .with_message("Error in function call with wrong arguments and return type mismatch in complex expression")
    .with_note("Note 1")
    .with_note("Note 2")
    .with_note("Note 3")
    // ... 10 more notes

// GOOD: Focus on the essential
Report::build(ReportKind::Error, span)
    .with_message("Function call has wrong arguments")
    .with_help("Expected 2 arguments, found 3")
```

**4. Inconsistent error codes:**
```rust
// BAD: Inconsistent naming
.with_code("E001")
.with_code("ERROR_002")
.with_code("type-mismatch")

// GOOD: Consistent scheme
.with_code("E0001")
.with_code("E0002")
.with_code("E0003")
```

**5. Not handling edge cases:**
```rust
// BAD: Assumes source exists
fn create_report(span: Span) -> Report<Span> {
    Report::build(ReportKind::Error, span)
        .with_label(Label::new(span)) // What if span is empty?
        .finish()
}

// GOOD: Handle edge cases
fn create_report(span: Span) -> Option<Report<Span>> {
    if span.is_empty() {
        return None;
    }
    
    Some(Report::build(ReportKind::Error, span)
        .with_label(Label::new(span))
        .finish())
}
```

### Performance Anti-patterns

**1. Creating sources repeatedly:**
```rust
// BAD: Recreates source for each report
for error in errors {
    let source = Source::from(&file_content); // Expensive!
    error.print(&source).unwrap();
}

// GOOD: Reuse source
let source = Source::from(&file_content);
for error in errors {
    error.print(&source).unwrap();
}
```

**2. Not using appropriate cache:**
```rust
// BAD: Loading files manually
let content = std::fs::read_to_string("file.rs").unwrap();
let source = Source::from(content);

// GOOD: Use FileCache
let mut cache = FileCache::default();
report.print(&mut cache).unwrap(); // Automatically loads and caches
```

**3. Generating too many reports:**
```rust
// BAD: No limit on error count
for item in items {
    if let Err(e) = process(item) {
        emit_error(e); // Could generate thousands of errors
    }
}

// GOOD: Limit error count
let mut error_count = 0;
for item in items {
    if error_count >= MAX_ERRORS {
        eprintln!("Too many errors, stopping...");
        break;
    }
    
    if let Err(e) = process(item) {
        emit_error(e);
        error_count += 1;
    }
}
```

By following these best practices, you'll create diagnostic tools that are performant, maintainable, and provide excellent user experience.