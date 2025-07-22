# Ariadne v0.5 - Comprehensive Documentation

A fancy compiler diagnostics crate for beautiful error reporting in Rust.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Creating Beautiful Error Messages](#creating-beautiful-error-messages)
3. [Label System and Spans](#label-system-and-spans)
4. [Color Customization](#color-customization)
5. [Multi-file Error Reporting](#multi-file-error-reporting)
6. [Language Server Integration](#language-server-integration)
7. [Custom Report Builders](#custom-report-builders)
8. [Performance Considerations](#performance-considerations)
9. [Complete API Reference](#complete-api-reference)

## Getting Started

Ariadne is a powerful diagnostics library that helps you create beautiful, informative error messages for compilers, interpreters, and other tools that process text.

### Installation

Add Ariadne to your `Cargo.toml`:

```toml
[dependencies]
ariadne = "0.5"

# Optional: Enable automatic color detection
ariadne = { version = "0.5", features = ["auto-color"] }
```

### Basic Usage

Here's a simple example to get you started:

```rust
use ariadne::{Color, Label, Report, ReportKind, Source};

fn main() {
    let source = "let x = 42 + \"hello\";";
    
    Report::build(ReportKind::Error, 12..19)
        .with_code(1)
        .with_message("Type mismatch")
        .with_label(
            Label::new(8..10)
                .with_message("This is a number")
                .with_color(Color::Blue)
        )
        .with_label(
            Label::new(13..20)
                .with_message("This is a string")
                .with_color(Color::Red)
        )
        .with_note("Cannot add number and string")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

This produces a beautiful error message with:
- Color-coded labels pointing to specific parts of the source
- Clear error messages and explanations
- Professional formatting that matches modern compiler standards

### Core Concepts

**Reports**: The main diagnostic container that holds error information, labels, and metadata.

**Labels**: Colored annotations that point to specific spans in your source code with explanatory messages.

**Spans**: Ranges in your source code (start..end offsets) that labels refer to.

**Sources**: Text content that reports are generated against, with support for single and multi-file scenarios.

## Creating Beautiful Error Messages

Ariadne excels at creating visually appealing and informative error messages. Here's how to make the most of its capabilities:

### Report Types

Ariadne supports several built-in report types:

```rust
use ariadne::{Report, ReportKind, Color};

// Error reports (red, for critical issues)
Report::build(ReportKind::Error, span)

// Warning reports (yellow, for potential issues)
Report::build(ReportKind::Warning, span)

// Advice reports (blue, for suggestions)
Report::build(ReportKind::Advice, span)

// Custom reports with your own styling
Report::build(ReportKind::Custom("Info", Color::Green), span)
```

### Rich Error Messages

Create comprehensive error messages with multiple components:

```rust
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

fn create_rich_error() {
    let mut colors = ColorGenerator::new();
    let primary = colors.next();
    let secondary = colors.next();
    
    let source = r#"
fn calculate(x: i32, y: str) -> i32 {
    x + y  // Error: cannot add i32 and str
}
"#;

    Report::build(ReportKind::Error, 45..50)
        .with_code("E0308")
        .with_message("Mismatched types in binary operation")
        .with_label(
            Label::new(41..42)
                .with_message(format!("This has type {}", "i32".fg(primary)))
                .with_color(primary)
        )
        .with_label(
            Label::new(45..46)
                .with_message(format!("This has type {}", "str".fg(secondary)))
                .with_color(secondary)
        )
        .with_label(
            Label::new(43..44)
                .with_message("Addition operator requires compatible types")
                .with_color(Color::Red)
                .with_order(1)
        )
        .with_note(format!(
            "Consider converting {} to {} or vice versa",
            "str".fg(secondary),
            "i32".fg(primary)
        ))
        .with_help("Use `.parse()` to convert string to number")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

### Message Formatting

Use the `Fmt` trait for rich text formatting:

```rust
use ariadne::{Color, Fmt};

// Colored text
let msg = format!("Expected {}, found {}", 
    "i32".fg(Color::Blue), 
    "str".fg(Color::Red)
);

// Bold text
let msg = format!("Error in {}", "function".bold());

// Combined formatting
let msg = format!("The {} {} is invalid", 
    "return".fg(Color::Yellow).bold(),
    "type".underline()
);
```

### Multi-line Messages

Support for complex, multi-line error descriptions:

```rust
Report::build(ReportKind::Error, span)
    .with_message("Complex parsing error")
    .with_note("This error occurred because:")
    .with_note("  1. The syntax is invalid")
    .with_note("  2. Missing closing bracket")
    .with_note("  3. Unexpected token found")
    .with_help("Try checking your bracket matching")
    .with_help("Consider using an IDE with syntax highlighting")
    .finish()
```

## Label System and Spans

Labels are the heart of Ariadne's visual appeal. They provide precise, colored annotations that guide users to the exact location of issues.

### Basic Labels

```rust
use ariadne::{Label, Color};

// Simple label with message
Label::new(10..15)
    .with_message("Invalid syntax here")

// Colored label
Label::new(10..15)
    .with_message("Type mismatch")
    .with_color(Color::Red)

// Label with priority (higher numbers = higher priority)
Label::new(10..15)
    .with_message("Primary issue")
    .with_priority(10)

// Label with custom ordering
Label::new(10..15)
    .with_message("Show this first")
    .with_order(-1)
```

### Span Types

Ariadne supports multiple span formats:

```rust
// Range spans (most common)
Label::new(10..15)

// Inclusive range spans
Label::new(10..=14)

// Tuple spans with source IDs (for multi-file)
Label::new(("file.rs", 10..15))

// Custom span types (implement the Span trait)
struct MySpan {
    file: String,
    start: usize,
    end: usize,
}

impl Span for MySpan {
    type SourceId = String;
    
    fn source(&self) -> &Self::SourceId { &self.file }
    fn start(&self) -> usize { self.start }
    fn end(&self) -> usize { self.end }
}
```

### Label Positioning and Priority

Control how labels are displayed and ordered:

```rust
use ariadne::{Label, Color};

// Labels with different priorities
// Higher priority labels get visual precedence
let primary_error = Label::new(10..15)
    .with_message("Main error")
    .with_color(Color::Red)
    .with_priority(100);

let secondary_info = Label::new(12..13)
    .with_message("Related issue")
    .with_color(Color::Yellow)
    .with_priority(50);

// Labels with custom ordering
// Lower order values appear first
let first_label = Label::new(20..25)
    .with_message("Show this first")
    .with_order(-10);

let second_label = Label::new(15..20)
    .with_message("Show this second")
    .with_order(0);
```

### Advanced Label Techniques

```rust
use ariadne::{Label, Color, ColorGenerator};

fn advanced_labeling_example() {
    let mut colors = ColorGenerator::new();
    
    // Generate consistent colors for related concepts
    let type_color = colors.next();
    let value_color = colors.next();
    let operator_color = colors.next();
    
    let source = "let result = x + y * z;";
    
    Report::build(ReportKind::Warning, 13..18)
        .with_message("Operator precedence warning")
        .with_label(
            Label::new(13..14)  // x
                .with_message(format!("Variable of type {}", "i32".fg(type_color)))
                .with_color(value_color)
        )
        .with_label(
            Label::new(15..16)  // +
                .with_message("Addition operator")
                .with_color(operator_color)
                .with_order(1)
        )
        .with_label(
            Label::new(17..18)  // y
                .with_message(format!("Variable of type {}", "i32".fg(type_color)))
                .with_color(value_color)
        )
        .with_label(
            Label::new(19..20)  // *
                .with_message("Multiplication (higher precedence)")
                .with_color(operator_color)
                .with_order(2)
        )
        .with_label(
            Label::new(21..22)  // z
                .with_message(format!("Variable of type {}", "i32".fg(type_color)))
                .with_color(value_color)
        )
        .with_note("Multiplication has higher precedence than addition")
        .with_help("Consider using parentheses: (x + y) * z")
        .finish()
}
```

## Color Customization

Ariadne provides extensive color customization options for creating visually consistent and accessible error messages.

### Built-in Colors

```rust
use ariadne::Color;

// Basic colors
Color::Red
Color::Green
Color::Blue
Color::Yellow
Color::Magenta
Color::Cyan
Color::White
Color::Black

// 8-bit colors (0-255)
Color::Fixed(196)  // Bright red
Color::Fixed(46)   // Bright green
Color::Fixed(21)   // Bright blue

// 24-bit RGB colors
Color::Rgb(255, 128, 0)  // Orange
Color::Rgb(128, 0, 255)  // Purple
```

### Color Generator

Automatically generate distinct colors for visual elements:

```rust
use ariadne::{ColorGenerator, Color};

fn use_color_generator() {
    let mut colors = ColorGenerator::new();
    
    // Generate distinct colors automatically
    let error_color = colors.next();      // First distinct color
    let warning_color = colors.next();    // Second distinct color
    let info_color = colors.next();       // Third distinct color
    
    // Colors are guaranteed to be visually distinct
    // Perfect for syntax highlighting or categorizing errors
}

// Custom color schemes
fn create_custom_scheme() -> Vec<Color> {
    vec![
        Color::Rgb(255, 99, 71),   // Tomato
        Color::Rgb(60, 179, 113),  // Medium sea green
        Color::Rgb(30, 144, 255),  // Dodger blue
        Color::Rgb(255, 165, 0),   // Orange
        Color::Rgb(147, 112, 219), // Medium purple
    ]
}
```

### Conditional Coloring

Control when colors are applied:

```rust
use ariadne::{Config, Color};

// Disable colors entirely
let config = Config::default().with_color(false);

// Conditional coloring based on environment
let use_color = std::env::var("NO_COLOR").is_err() && 
                std::io::IsTerminal::is_terminal(&std::io::stderr());

let config = Config::default().with_color(use_color);

Report::build(ReportKind::Error, span)
    .with_config(config)
    .with_label(
        Label::new(span)
            .with_color(Color::Red)  // Only applied if colors enabled
    )
    .finish()
```

### Accessibility Considerations

```rust
use ariadne::{Color, Config, CharSet};

// High contrast color scheme for accessibility
fn accessible_colors() -> Vec<Color> {
    vec![
        Color::Rgb(255, 0, 0),     // Pure red
        Color::Rgb(0, 255, 0),     // Pure green  
        Color::Rgb(0, 0, 255),     // Pure blue
        Color::Rgb(255, 255, 0),   // Pure yellow
        Color::Rgb(255, 0, 255),   // Pure magenta
    ]
}

// ASCII-only output for better compatibility
let config = Config::default()
    .with_char_set(CharSet::Ascii)
    .with_color(true);

// Support for screen readers and text-only environments
let config = Config::default()
    .with_color(false)
    .with_char_set(CharSet::Ascii)
    .with_compact(true);
```

## Multi-file Error Reporting

Ariadne excels at handling errors that span multiple files, making it perfect for complex projects and modular codebases.

### Basic Multi-file Setup

```rust
use ariadne::{sources, Label, Report, ReportKind};

fn multi_file_example() {
    // Create a cache of multiple source files
    let cache = sources(vec![
        ("main.rs", "mod utils;\nfn main() { utils::broken_function(); }"),
        ("utils.rs", "pub fn broken_function() {\n    undefined_variable\n}"),
    ]);
    
    Report::build(ReportKind::Error, ("utils.rs", 35..52))
        .with_code("E0425")
        .with_message("Cannot find value `undefined_variable` in this scope")
        .with_label(
            Label::new(("utils.rs", 35..52))
                .with_message("Not found in this scope")
                .with_color(Color::Red)
        )
        .with_label(
            Label::new(("main.rs", 25..48))
                .with_message("Called from here")
                .with_color(Color::Blue)
        )
        .with_note("Consider defining this variable or importing it")
        .finish()
        .print(cache)
        .unwrap();
}
```

### File Cache Types

Ariadne provides several cache implementations:

```rust
use ariadne::{FileCache, FnCache, Source, sources};
use std::collections::HashMap;

// 1. FileCache - reads from filesystem
let mut file_cache = FileCache::default();
// Automatically loads files as needed

// 2. FnCache - custom loading function
let fn_cache = FnCache::new(|path: &str| {
    std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path, e))
});

// 3. sources() - from in-memory collection
let memory_cache = sources(vec![
    ("file1.rs", "content1"),
    ("file2.rs", "content2"),
]);

// 4. Custom cache implementation
struct DatabaseCache {
    // Your custom storage
}

impl Cache<String> for DatabaseCache {
    type Storage = String;
    
    fn fetch(&mut self, id: &String) -> Result<&Source<String>, impl std::fmt::Debug> {
        // Your custom loading logic
        todo!()
    }
    
    fn display<'a>(&self, id: &'a String) -> Option<impl std::fmt::Display + 'a> {
        Some(id)
    }
}
```

### Cross-file References

Create sophisticated error reports that show relationships between files:

```rust
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, sources};

fn cross_file_analysis() {
    let mut colors = ColorGenerator::new();
    let def_color = colors.next();
    let use_color = colors.next();
    let error_color = colors.next();
    
    let cache = sources(vec![
        ("types.rs", r#"
pub struct User {
    pub name: String,
    pub age: u32,
}
"#),
        ("main.rs", r#"
use types::User;

fn process_user(user: User) {
    println!("User {} is {} years old", user.name, user.age);
    user.age = 25; // Error: cannot assign to immutable field
}
"#),
    ]);
    
    Report::build(ReportKind::Error, ("main.rs", 95..104))
        .with_code("E0594")
        .with_message("Cannot assign to immutable field")
        .with_label(
            Label::new(("main.rs", 95..104))
                .with_message("Cannot assign to this field")
                .with_color(error_color)
        )
        .with_label(
            Label::new(("main.rs", 60..64))
                .with_message(format!("Parameter {} is not mutable", "user".fg(use_color)))
                .with_color(use_color)
        )
        .with_label(
            Label::new(("types.rs", 45..48))
                .with_message(format!("Field {} defined here", "age".fg(def_color)))
                .with_color(def_color)
        )
        .with_note("Fields of immutable structs cannot be modified")
        .with_help("Consider making the parameter mutable: `mut user: User`")
        .finish()
        .print(cache)
        .unwrap();
}
```

### Large Project Support

Efficiently handle large codebases:

```rust
use ariadne::{FileCache, Report, ReportKind};
use std::path::Path;

fn large_project_setup() {
    // FileCache automatically manages memory and loading
    let mut cache = FileCache::default();
    
    // Reports can reference any file in your project
    Report::build(ReportKind::Error, ("src/lib.rs", 100..110))
        .with_label(
            Label::new(("src/utils/helper.rs", 50..60))
                .with_message("Related definition")
        )
        .with_label(
            Label::new(("tests/integration_test.rs", 200..210))
                .with_message("Usage example")
        )
        .finish()
        .print(&mut cache)
        .unwrap();
}

// Custom path resolution
fn with_custom_paths() {
    let cache = FnCache::new(|relative_path: &str| {
        let full_path = Path::new("project_root").join(relative_path);
        std::fs::read_to_string(full_path)
    });
    
    // Now you can use relative paths in your reports
    Report::build(ReportKind::Error, ("src/main.rs", 0..10))
        .finish()
        .print(cache)
        .unwrap();
}
```

## Language Server Integration

Ariadne integrates seamlessly with Language Server Protocol (LSP) implementations, making it perfect for IDE integration.

### LSP Diagnostic Conversion

Convert Ariadne reports to LSP diagnostics:

```rust
use ariadne::{Report, ReportKind, Label, Color};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

fn report_to_lsp_diagnostic(report: &Report<(String, std::ops::Range<usize>)>) -> Diagnostic {
    // Convert Ariadne severity to LSP severity
    let severity = match report.kind {
        ReportKind::Error => Some(DiagnosticSeverity::ERROR),
        ReportKind::Warning => Some(DiagnosticSeverity::WARNING),
        ReportKind::Advice => Some(DiagnosticSeverity::HINT),
        ReportKind::Custom(_, _) => Some(DiagnosticSeverity::INFORMATION),
    };
    
    // Convert span to LSP range
    let range = span_to_lsp_range(&report.span);
    
    Diagnostic {
        range,
        severity,
        code: report.code.clone().map(|c| lsp_types::NumberOrString::String(c)),
        source: Some("your-language-server".to_string()),
        message: report.msg.clone().unwrap_or_default(),
        related_information: Some(
            report.labels.iter()
                .map(|label| lsp_types::DiagnosticRelatedInformation {
                    location: lsp_types::Location {
                        uri: file_path_to_uri(label.span.source()),
                        range: span_to_lsp_range(&label.span),
                    },
                    message: label.display_info.msg.clone().unwrap_or_default(),
                })
                .collect()
        ),
        ..Default::default()
    }
}

fn span_to_lsp_range(span: &(String, std::ops::Range<usize>)) -> Range {
    // Convert character offsets to line/column positions
    // This requires access to the source text
    todo!("Implement offset to position conversion")
}
```

### Real-time Error Reporting

Integrate with language server for real-time diagnostics:

```rust
use ariadne::{Report, ReportKind, Label, Color, Source};
use std::collections::HashMap;

struct LanguageServer {
    sources: HashMap<String, Source>,
    diagnostics: HashMap<String, Vec<Report<(String, std::ops::Range<usize>)>>>,
}

impl LanguageServer {
    fn update_document(&mut self, uri: String, content: String) {
        // Update source
        self.sources.insert(uri.clone(), Source::from(content));
        
        // Re-analyze and generate diagnostics
        let diagnostics = self.analyze_document(&uri);
        self.diagnostics.insert(uri.clone(), diagnostics);
        
        // Send diagnostics to client
        self.send_diagnostics(uri);
    }
    
    fn analyze_document(&self, uri: &str) -> Vec<Report<(String, std::ops::Range<usize>)>> {
        let mut reports = Vec::new();
        
        // Your analysis logic here
        // Example: syntax error
        reports.push(
            Report::build(ReportKind::Error, (uri.to_string(), 10..15))
                .with_code("SYNTAX001")
                .with_message("Unexpected token")
                .with_label(
                    Label::new((uri.to_string(), 10..15))
                        .with_message("Expected ';' here")
                        .with_color(Color::Red)
                )
                .finish()
        );
        
        reports
    }
    
    fn send_diagnostics(&self, uri: String) {
        let reports = self.diagnostics.get(&uri).unwrap_or(&vec![]);
        
        // Convert to LSP diagnostics and send
        let lsp_diagnostics: Vec<_> = reports.iter()
            .map(|report| report_to_lsp_diagnostic(report))
            .collect();
            
        // Send via LSP protocol
        // self.client.publish_diagnostics(uri, lsp_diagnostics);
    }
}
```

### IDE Integration Features

Support advanced IDE features:

```rust
use ariadne::{Report, ReportKind, Label, Color};

// Code actions and quick fixes
fn generate_code_action(report: &Report<(String, std::ops::Range<usize>)>) -> Vec<lsp_types::CodeAction> {
    let mut actions = Vec::new();
    
    match report.code.as_deref() {
        Some("MISSING_SEMICOLON") => {
            actions.push(lsp_types::CodeAction {
                title: "Add semicolon".to_string(),
                kind: Some(lsp_types::CodeActionKind::QUICKFIX),
                edit: Some(lsp_types::WorkspaceEdit {
                    changes: Some({
                        let mut changes = HashMap::new();
                        changes.insert(
                            file_path_to_uri(report.span.source()),
                            vec![lsp_types::TextEdit {
                                range: span_to_lsp_range(&report.span),
                                new_text: ";".to_string(),
                            }]
                        );
                        changes
                    }),
                    ..Default::default()
                }),
                ..Default::default()
            });
        }
        _ => {}
    }
    
    actions
}

// Hover information
fn generate_hover_info(position: Position, source: &Source) -> Option<lsp_types::Hover> {
    // Find relevant diagnostic at position
    // Return formatted hover information
    Some(lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: "**Error**: Type mismatch\n\nExpected `i32`, found `str`".to_string(),
        }),
        range: None,
    })
}
```

## Custom Report Builders

Create specialized report builders for your specific use cases and domain requirements.

### Custom Report Types

```rust
use ariadne::{Report, ReportKind, Label, Color, Config};

// Domain-specific report builder
struct CompilerReportBuilder {
    reports: Vec<Report<(String, std::ops::Range<usize>)>>,
    config: Config,
}

impl CompilerReportBuilder {
    fn new() -> Self {
        Self {
            reports: Vec::new(),
            config: Config::default(),
        }
    }
    
    // Specialized methods for common compiler errors
    fn type_mismatch(&mut self, span: (String, std::ops::Range<usize>), expected: &str, found: &str) {
        let report = Report::build(ReportKind::Error, span.clone())
            .with_code("E0308")
            .with_message(format!("Mismatched types: expected `{}`, found `{}`", expected, found))
            .with_label(
                Label::new(span)
                    .with_message(format!("Expected `{}`, found `{}`", expected, found))
                    .with_color(Color::Red)
            )
            .with_note(format!("Consider converting `{}` to `{}`", found, expected))
            .finish();
            
        self.reports.push(report);
    }
    
    fn undefined_variable(&mut self, span: (String, std::ops::Range<usize>), name: &str) {
        let report = Report::build(ReportKind::Error, span.clone())
            .with_code("E0425")
            .with_message(format!("Cannot find value `{}` in this scope", name))
            .with_label(
                Label::new(span)
                    .with_message("Not found in this scope")
                    .with_color(Color::Red)
            )
            .with_help("Consider defining this variable or importing it")
            .finish();
            
        self.reports.push(report);
    }
    
    fn unused_variable(&mut self, span: (String, std::ops::Range<usize>), name: &str) {
        let report = Report::build(ReportKind::Warning, span.clone())
            .with_code("W0001")
            .with_message(format!("Unused variable `{}`", name))
            .with_label(
                Label::new(span)
                    .with_message("This variable is never used")
                    .with_color(Color::Yellow)
            )
            .with_help(format!("Consider prefixing with underscore: `_{}`", name))
            .finish();
            
        self.reports.push(report);
    }
    
    // Batch operations
    fn print_all<C: Cache<String>>(&self, cache: C) -> std::io::Result<()> {
        for report in &self.reports {
            report.print(&cache)?;
        }
        Ok(())
    }
    
    fn clear(&mut self) {
        self.reports.clear();
    }
}
```

### Template-based Reports

Create reusable report templates:

```rust
use ariadne::{Report, ReportKind, Label, Color};

struct ReportTemplate {
    kind: ReportKind<'static>,
    code: Option<String>,
    message_template: String,
    label_templates: Vec<LabelTemplate>,
    notes: Vec<String>,
    help: Vec<String>,
}

struct LabelTemplate {
    message_template: String,
    color: Color,
    priority: i32,
}

impl ReportTemplate {
    fn syntax_error() -> Self {
        Self {
            kind: ReportKind::Error,
            code: Some("SYNTAX".to_string()),
            message_template: "Syntax error: {error}".to_string(),
            label_templates: vec![
                LabelTemplate {
                    message_template: "Unexpected {token}".to_string(),
                    color: Color::Red,
                    priority: 100,
                }
            ],
            notes: vec!["Check your syntax carefully".to_string()],
            help: vec!["Refer to the language documentation".to_string()],
        }
    }
    
    fn instantiate(&self, span: (String, std::ops::Range<usize>), params: &HashMap<String, String>) -> Report<(String, std::ops::Range<usize>)> {
        let mut builder = Report::build(self.kind, span.clone());
        
        if let Some(code) = &self.code {
            builder = builder.with_code(code);
        }
        
        // Template substitution
        let message = self.substitute_template(&self.message_template, params);
        builder = builder.with_message(message);
        
        // Add labels
        for (i, label_template) in self.label_templates.iter().enumerate() {
            let label_message = self.substitute_template(&label_template.message_template, params);
            builder = builder.with_label(
                Label::new(span.clone())
                    .with_message(label_message)
                    .with_color(label_template.color)
                    .with_priority(label_template.priority)
            );
        }
        
        // Add notes and help
        for note in &self.notes {
            builder = builder.with_note(self.substitute_template(note, params));
        }
        
        for help in &self.help {
            builder = builder.with_help(self.substitute_template(help, params));
        }
        
        builder.finish()
    }
    
    fn substitute_template(&self, template: &str, params: &HashMap<String, String>) -> String {
        let mut result = template.to_string();
        for (key, value) in params {
            result = result.replace(&format!("{{{}}}", key), value);
        }
        result
    }
}

// Usage
fn use_template() {
    let template = ReportTemplate::syntax_error();
    let mut params = HashMap::new();
    params.insert("error".to_string(), "unexpected token".to_string());
    params.insert("token".to_string(), "';'".to_string());
    
    let report = template.instantiate(
        ("file.rs".to_string(), 10..15),
        &params
    );
}
```

### Fluent Interface Builder

Create a fluent interface for complex report building:

```rust
use ariadne::{Report, ReportKind, Label, Color, Config};

struct FluentReportBuilder<S> {
    report: Report<S>,
}

impl<S: Span> FluentReportBuilder<S> {
    fn error(span: S) -> Self {
        Self {
            report: Report::build(ReportKind::Error, span).finish(),
        }
    }
    
    fn warning(span: S) -> Self {
        Self {
            report: Report::build(ReportKind::Warning, span).finish(),
        }
    }
    
    fn code(mut self, code: impl std::fmt::Display) -> Self {
        // Rebuild with code
        self
    }
    
    fn message(mut self, msg: impl ToString) -> Self {
        // Rebuild with message
        self
    }
    
    fn primary_label(mut self, span: S, msg: impl ToString) -> Self {
        // Add primary label
        self
    }
    
    fn secondary_label(mut self, span: S, msg: impl ToString) -> Self {
        // Add secondary label
        self
    }
    
    fn note(mut self, note: impl ToString) -> Self {
        // Add note
        self
    }
    
    fn help(mut self, help: impl ToString) -> Self {
        // Add help
        self
    }
    
    fn finish(self) -> Report<S> {
        self.report
    }
}

// Usage with fluent interface
fn fluent_example() {
    let report = FluentReportBuilder::error(("file.rs", 10..15))
        .code("E0001")
        .message("Type mismatch")
        .primary_label(("file.rs", 10..12), "Expected i32")
        .secondary_label(("file.rs", 13..15), "Found str")
        .note("Types must match in this context")
        .help("Consider converting the string to a number")
        .finish();
}
```

## Performance Considerations

Optimize Ariadne usage for large-scale applications and performance-critical scenarios.

### Efficient Source Management

```rust
use ariadne::{Source, FileCache, FnCache};
use std::sync::Arc;

// Use Arc<str> for shared source content
fn shared_sources() {
    let content: Arc<str> = Arc::from("large source content...");
    let source = Source::from(content.clone());
    
    // Multiple reports can share the same source without copying
    // This is especially important for large files
}

// Lazy loading with FnCache
fn lazy_loading() {
    let cache = FnCache::new(|path: &str| {
        // Only load files when actually needed
        std::fs::read_to_string(path)
    });
    
    // Files are loaded on-demand and cached
}

// Pre-populate cache for known files
fn preloaded_cache() {
    let mut sources = std::collections::HashMap::new();
    
    // Load frequently accessed files upfront
    sources.insert("common.rs".to_string(), Source::from("..."));
    sources.insert("types.rs".to_string(), Source::from("..."));
    
    let cache = FnCache::new(|_| Err("File not found"))
        .with_sources(sources);
}
```

### Batch Report Processing

```rust
use ariadne::{Report, ReportKind, Label, Color};

struct BatchReporter<S> {
    reports: Vec<Report<S>>,
    max_reports: usize,
}

impl<S: Span> BatchReporter<S> {
    fn new(max_reports: usize) -> Self {
        Self {
            reports: Vec::with_capacity(max_reports),
            max_reports,
        }
    }
    
    fn add_report(&mut self, report: Report<S>) {
        if self.reports.len() < self.max_reports {
            self.reports.push(report);
        }
    }
    
    fn flush<C: Cache<S::SourceId>>(&mut self, cache: C) -> std::io::Result<()> {
        // Sort reports by severity and location for better output
        self.reports.sort_by_key(|r| (r.kind, r.span.start()));
        
        // Print all reports in batch
        for report in &self.reports {
            report.print(&cache)?;
        }
        
        self.reports.clear();
        Ok(())
    }
}

// Limit error reporting to prevent spam
fn error_limiting() {
    let mut reporter = BatchReporter::new(10); // Max 10 errors
    
    // Add reports...
    // Only the first 10 will be stored and displayed
}
```

### Memory-Efficient Configuration

```rust
use ariadne::{Config, CharSet, IndexType};

// Optimize for memory usage
fn memory_optimized_config() -> Config {
    Config::default()
        .with_compact(true)           // Reduce whitespace
        .with_index_type(IndexType::Byte) // Use byte offsets (O(1) lookups)
        .with_char_set(CharSet::Ascii)    // ASCII-only characters
        .with_color(false)            // Disable colors if not needed
}

// Optimize for performance
fn performance_optimized_config() -> Config {
    Config::default()
        .with_index_type(IndexType::Byte) // Faster than char indexing
        .with_multiline_arrows(false)     // Simpler rendering
        .with_underlines(false)           // Skip underline calculations
}
```

### Streaming and Incremental Processing

```rust
use ariadne::{Report, ReportKind, Source};
use std::io::Write;

// Stream reports to avoid memory buildup
struct StreamingReporter<W: Write> {
    writer: W,
    config: Config,
}

impl<W: Write> StreamingReporter<W> {
    fn new(writer: W) -> Self {
        Self {
            writer,
            config: Config::default(),
        }
    }
    
    fn report<S: Span, C: Cache<S::SourceId>>(&mut self, report: Report<S>, cache: C) -> std::io::Result<()> {
        // Write report immediately, don't store
        report.write(cache, &mut self.writer)
    }
}

// Incremental source updates
struct IncrementalSource {
    content: String,
    source: Option<Source>,
    dirty: bool,
}

impl IncrementalSource {
    fn update(&mut self, new_content: String) {
        if self.content != new_content {
            self.content = new_content;
            self.dirty = true;
            self.source = None; // Invalidate cached source
        }
    }
    
    fn get_source(&mut self) -> &Source {
        if self.source.is_none() || self.dirty {
            self.source = Some(Source::from(&self.content));
            self.dirty = false;
        }
        self.source.as_ref().unwrap()
    }
}
```

### Profiling and Benchmarking

```rust
use ariadne::{Report, ReportKind, Source};
use std::time::Instant;

// Benchmark report generation
fn benchmark_reports() {
    let source = Source::from("large source content...");
    let start = Instant::now();
    
    for i in 0..1000 {
        let report = Report::build(ReportKind::Error, i..i+10)
            .with_message("Test error")
            .finish();
        
        // Measure just the formatting, not I/O
        let mut buffer = Vec::new();
        report.write(&source, &mut buffer).unwrap();
    }
    
    let duration = start.elapsed();
    println!("Generated 1000 reports in {:?}", duration);
}

// Memory usage tracking
fn track_memory_usage() {
    let initial_memory = get_memory_usage();
    
    // Create many reports
    let mut reports = Vec::new();
    for i in 0..10000 {
        reports.push(
            Report::build(ReportKind::Error, i..i+10)
                .with_message("Test error")
                .finish()
        );
    }
    
    let final_memory = get_memory_usage();
    println!("Memory used: {} bytes", final_memory - initial_memory);
}

#[cfg(target_os = "linux")]
fn get_memory_usage() -> usize {
    // Platform-specific memory tracking
    std::fs::read_to_string("/proc/self/status")
        .unwrap()
        .lines()
        .find(|line| line.starts_with("VmRSS:"))
        .and_then(|line| line.split_whitespace().nth(1))
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(0) * 1024 // Convert KB to bytes
}
```

## Complete API Reference

### Core Types

#### `Report<S: Span>`

The main diagnostic container.

**Construction:**
```rust
Report::build(kind: ReportKind, span: S) -> ReportBuilder<S>
```

**Methods:**
- `print<C: Cache<S::SourceId>>(cache: C) -> io::Result<()>` - Print to stdout
- `eprint<C: Cache<S::SourceId>>(cache: C) -> io::Result<()>` - Print to stderr  
- `write<C: Cache<S::SourceId>, W: Write>(cache: C, writer: W) -> io::Result<()>` - Write to custom writer

#### `ReportBuilder<S: Span>`

Builder for constructing reports.

**Methods:**
- `with_code<C: Display>(code: C) -> Self` - Add error code
- `with_message<M: ToString>(msg: M) -> Self` - Set main message
- `with_label(label: Label<S>) -> Self` - Add a label
- `with_labels<I: IntoIterator<Item = Label<S>>>(labels: I) -> Self` - Add multiple labels
- `with_note<N: ToString>(note: N) -> Self` - Add a note
- `with_help<H: ToString>(help: H) -> Self` - Add help text
- `with_config(config: Config) -> Self` - Set configuration
- `finish() -> Report<S>` - Build the final report

#### `ReportKind`

Enum defining report types.

**Variants:**
- `Error` - Critical errors (red)
- `Warning` - Potential issues (yellow)  
- `Advice` - Suggestions (blue)
- `Custom(&str, Color)` - Custom type with name and color

#### `Label<S: Span>`

Colored annotations pointing to source locations.

**Construction:**
```rust
Label::new(span: S) -> Self
```

**Methods:**
- `with_message<M: ToString>(msg: M) -> Self` - Set label message
- `with_color(color: Color) -> Self` - Set label color
- `with_priority(priority: i32) -> Self` - Set display priority (higher = more important)
- `with_order(order: i32) -> Self` - Set display order (lower = shown first)

### Span Types

#### `Span` Trait

Defines source location interface.

**Required Methods:**
- `source() -> &Self::SourceId` - Get source identifier
- `start() -> usize` - Get start offset
- `end() -> usize` - Get end offset

**Default Methods:**
- `len() -> usize` - Get span length
- `is_empty() -> bool` - Check if span is empty
- `contains(offset: usize) -> bool` - Check if offset is within span

**Built-in Implementations:**
- `Range<usize>` - Simple range with `()` source ID
- `RangeInclusive<usize>` - Inclusive range with `()` source ID
- `(Id, Range<usize>)` - Range with custom source ID
- `(Id, RangeInclusive<usize>)` - Inclusive range with custom source ID

### Source Management

#### `Source<I: AsRef<str>>`

Represents source text with line information.

**Construction:**
```rust
Source::from(text: I) -> Self
```

**Methods:**
- `text() -> &str` - Get full source text
- `len() -> usize` - Get character count
- `is_empty() -> bool` - Check if source is empty
- `chars() -> impl Iterator<Item = char>` - Iterate over characters
- `line(idx: usize) -> Option<Line>` - Get specific line
- `lines() -> impl ExactSizeIterator<Item = Line>` - Iterate over lines
- `get_offset_line(offset: usize) -> Option<(Line, usize, usize)>` - Get line containing offset
- `get_line_range<S: Span>(span: &S) -> Range<usize>` - Get line range for span
- `with_display_line_offset(offset: usize) -> Self` - Add offset to displayed line numbers

#### `Line`

Represents a single line in source text.

**Methods:**
- `offset() -> usize` - Get character offset from start of source
- `len() -> usize` - Get character length of line
- `is_empty() -> bool` - Check if line is empty
- `span() -> Range<usize>` - Get offset range of line

#### `Cache<Id>` Trait

Interface for source storage and retrieval.

**Required Methods:**
- `fetch(&mut self, id: &Id) -> Result<&Source<Self::Storage>, impl Debug>` - Load source by ID
- `display<'a>(&self, id: &'a Id) -> Option<impl Display + 'a>` - Format ID for display

**Built-in Implementations:**

##### `FileCache`
Loads sources from filesystem.
```rust
let mut cache = FileCache::default();
```

##### `FnCache<Id, F, I>`
Loads sources using custom function.
```rust
let cache = FnCache::new(|path: &str| std::fs::read_to_string(path));
```

##### `sources<Id, S, I>(iter: I) -> impl Cache<Id>`
Creates cache from iterator of (ID, content) pairs.
```rust
let cache = sources(vec![
    ("file1.rs", "content1"),
    ("file2.rs", "content2"),
]);
```

### Colors and Formatting

#### `Color`

Color specification for labels and text.

**Variants:**
- `Black`, `Red`, `Green`, `Yellow`, `Blue`, `Magenta`, `Cyan`, `White` - Basic colors
- `Fixed(u8)` - 8-bit color (0-255)
- `Rgb(u8, u8, u8)` - 24-bit RGB color

#### `ColorGenerator`

Generates visually distinct colors.

**Methods:**
- `new() -> Self` - Create new generator
- `next() -> Color` - Get next distinct color

#### `Fmt` Trait

Text formatting for rich output.

**Methods:**
- `fg(color: Color) -> impl Display` - Set foreground color
- `bold() -> impl Display` - Make text bold
- `underline() -> impl Display` - Underline text

### Configuration

#### `Config`

Controls report appearance and behavior.

**Construction:**
```rust
Config::default() // or Config::new()
```

**Methods:**
- `with_cross_gap(cross_gap: bool) -> Self` - Enable gaps at line crossings
- `with_label_attach(attach: LabelAttach) -> Self` - Set label attachment point
- `with_compact(compact: bool) -> Self` - Enable compact mode
- `with_underlines(underlines: bool) -> Self` - Enable span underlines
- `with_multiline_arrows(arrows: bool) -> Self` - Enable multiline arrows
- `with_color(color: bool) -> Self` - Enable/disable colors
- `with_tab_width(width: usize) -> Self` - Set tab character width
- `with_char_set(char_set: CharSet) -> Self` - Set character set
- `with_index_type(index_type: IndexType) -> Self` - Set indexing type

#### `LabelAttach`

Label arrow attachment points.

**Variants:**
- `Start` - Attach to start of span
- `Middle` - Attach to middle of span
- `End` - Attach to end of span

#### `CharSet`

Character sets for drawing.

**Variants:**
- `Unicode` - Unicode box-drawing characters
- `Ascii` - ASCII-only characters

#### `IndexType`

Indexing strategies for performance.

**Variants:**
- `Char` - Character-based indexing (default, may be O(n))
- `Byte` - Byte-based indexing (O(1) but requires UTF-8 awareness)

### Utility Functions

#### `sources<Id, S, I>(iter: I) -> impl Cache<Id>`

Create a cache from an iterator of (ID, source) pairs.

```rust
let cache = sources(vec![
    ("main.rs", include_str!("main.rs")),
    ("lib.rs", include_str!("lib.rs")),
]);
```

### Feature Flags

#### `auto-color`

Enables automatic color detection via `concolor` crate.

```toml
[dependencies]
ariadne = { version = "0.5", features = ["auto-color"] }
```

#### `concolor`

Enables integration with `concolor` for global color control.

```toml
[dependencies]
ariadne = { version = "0.5", features = ["concolor"] }
```

### Complete Example

Here's a comprehensive example showcasing most of Ariadne's features:

```rust
use ariadne::{
    Color, ColorGenerator, Config, CharSet, LabelAttach,
    Label, Report, ReportKind, Source, sources, Fmt
};

fn comprehensive_example() {
    // Setup color generator for consistent theming
    let mut colors = ColorGenerator::new();
    let error_color = colors.next();
    let warning_color = colors.next();
    let info_color = colors.next();
    
    // Configure appearance
    let config = Config::default()
        .with_color(true)
        .with_char_set(CharSet::Unicode)
        .with_label_attach(LabelAttach::Middle)
        .with_compact(false)
        .with_tab_width(4);
    
    // Multi-file source cache
    let cache = sources(vec![
        ("main.rs", r#"
mod utils;
use utils::process_data;

fn main() {
    let data = "hello world";
    let result = process_data(data, 42);
    println!("Result: {}", result);
}
"#),
        ("utils.rs", r#"
pub fn process_data(text: &str, count: i32) -> String {
    if count > text.len() {
        panic!("Count too large!");
    }
    text.chars().take(count).collect()
}
"#),
    ]);
    
    // Complex error report with multiple labels and cross-file references
    Report::build(ReportKind::Error, ("utils.rs", 45..60))
        .with_code("E0308")
        .with_message("Type mismatch in comparison")
        .with_config(config)
        .with_label(
            Label::new(("utils.rs", 45..50))
                .with_message(format!("This has type {}", "i32".fg(error_color)))
                .with_color(error_color)
                .with_priority(100)
        )
        .with_label(
            Label::new(("utils.rs", 53..62))
                .with_message(format!("This has type {}", "usize".fg(warning_color)))
                .with_color(warning_color)
                .with_priority(90)
        )
        .with_label(
            Label::new(("main.rs", 89..91))
                .with_message(format!("Value {} passed here", "42".fg(info_color)))
                .with_color(info_color)
                .with_priority(50)
        )
        .with_note(format!(
            "Cannot compare {} and {} directly",
            "i32".fg(error_color),
            "usize".fg(warning_color)
        ))
        .with_note("Consider using explicit type conversion")
        .with_help("Try: `count as usize > text.len()`")
        .with_help("Or: `count > text.len() as i32`")
        .finish()
        .print(cache)
        .unwrap();
}
```

This documentation covers all major aspects of Ariadne v0.5, providing both practical examples and comprehensive API reference information. The library's strength lies in its flexibility, performance, and beautiful output that helps developers create professional-quality diagnostic tools.