# Ariadne Examples Collection

This document contains practical examples demonstrating Ariadne's capabilities for different use cases.

## Table of Contents

1. [Basic Examples](#basic-examples)
2. [Compiler Error Examples](#compiler-error-examples)
3. [Multi-file Examples](#multi-file-examples)
4. [Advanced Formatting Examples](#advanced-formatting-examples)
5. [Language Server Examples](#language-server-examples)
6. [Performance Examples](#performance-examples)

## Basic Examples

### Simple Syntax Error

```rust
use ariadne::{Color, Label, Report, ReportKind, Source};

fn simple_syntax_error() {
    let source = "let x = 42 +";
    
    Report::build(ReportKind::Error, 11..13)
        .with_code("E0001")
        .with_message("Unexpected end of input")
        .with_label(
            Label::new(11..13)
                .with_message("Expected expression after '+'")
                .with_color(Color::Red)
        )
        .with_help("Try adding a value: `let x = 42 + 1;`")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

### Type Mismatch Error

```rust
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

fn type_mismatch_error() {
    let mut colors = ColorGenerator::new();
    let expected_color = colors.next();
    let found_color = colors.next();
    
    let source = r#"let age: i32 = "twenty-five";"#;
    
    Report::build(ReportKind::Error, 15..27)
        .with_code("E0308")
        .with_message("Mismatched types")
        .with_label(
            Label::new(9..12)
                .with_message(format!("Expected type {}", "i32".fg(expected_color)))
                .with_color(expected_color)
        )
        .with_label(
            Label::new(15..27)
                .with_message(format!("Found type {}", "&str".fg(found_color)))
                .with_color(found_color)
        )
        .with_note(format!(
            "Cannot assign {} to variable of type {}",
            "&str".fg(found_color),
            "i32".fg(expected_color)
        ))
        .with_help("Consider parsing the string: `\"25\".parse::<i32>()?`")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

### Warning Example

```rust
use ariadne::{Color, Label, Report, ReportKind, Source};

fn unused_variable_warning() {
    let source = r#"
fn main() {
    let unused_var = 42;
    println!("Hello, world!");
}
"#;
    
    Report::build(ReportKind::Warning, 20..30)
        .with_code("W0001")
        .with_message("Unused variable")
        .with_label(
            Label::new(20..30)
                .with_message("This variable is never used")
                .with_color(Color::Yellow)
        )
        .with_note("Variables that are not used can be removed")
        .with_help("Consider prefixing with underscore: `_unused_var`")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

## Compiler Error Examples

### Function Signature Mismatch

```rust
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

fn function_signature_mismatch() {
    let mut colors = ColorGenerator::new();
    let def_color = colors.next();
    let call_color = colors.next();
    let error_color = Color::Red;
    
    let source = r#"
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let result = add(42, "hello", true);
}
"#;
    
    Report::build(ReportKind::Error, 75..94)
        .with_code("E0061")
        .with_message("Function called with wrong number of arguments")
        .with_label(
            Label::new(4..29)
                .with_message(format!("Function {} defined here", "add".fg(def_color)))
                .with_color(def_color)
        )
        .with_label(
            Label::new(75..78)
                .with_message("Function called here")
                .with_color(call_color)
        )
        .with_label(
            Label::new(79..81)
                .with_message("Argument 1: correct type")
                .with_color(Color::Green)
        )
        .with_label(
            Label::new(83..90)
                .with_message(format!("Argument 2: expected {}, found {}", 
                    "i32".fg(def_color), "&str".fg(error_color)))
                .with_color(error_color)
        )
        .with_label(
            Label::new(92..96)
                .with_message("Argument 3: unexpected extra argument")
                .with_color(error_color)
        )
        .with_note("This function takes 2 arguments but 3 were supplied")
        .with_help("Remove the extra argument or check the function signature")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

### Borrow Checker Error

```rust
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

fn borrow_checker_error() {
    let mut colors = ColorGenerator::new();
    let borrow_color = colors.next();
    let move_color = colors.next();
    let use_color = colors.next();
    
    let source = r#"
fn main() {
    let s = String::from("hello");
    let r1 = &s;
    let s2 = s;  // Move occurs here
    println!("{}", r1);  // Error: use after move
}
"#;
    
    Report::build(ReportKind::Error, 85..87)
        .with_code("E0382")
        .with_message("Borrow of moved value")
        .with_label(
            Label::new(20..21)
                .with_message(format!("Variable {} defined here", "s".fg(borrow_color)))
                .with_color(borrow_color)
        )
        .with_label(
            Label::new(47..49)
                .with_message(format!("Immutable borrow of {} occurs here", "s".fg(borrow_color)))
                .with_color(borrow_color)
        )
        .with_label(
            Label::new(59..60)
                .with_message(format!("Value {} moved here", "s".fg(move_color)))
                .with_color(move_color)
                .with_priority(100)
        )
        .with_label(
            Label::new(85..87)
                .with_message(format!("Use of moved value {} here", "r1".fg(use_color)))
                .with_color(use_color)
                .with_priority(100)
        )
        .with_note(format!("Value {} moved into {} here", "s".fg(move_color), "s2".fg(move_color)))
        .with_help("Consider cloning the value: `let s2 = s.clone();`")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

### Pattern Matching Error

```rust
use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};

fn pattern_matching_error() {
    let mut colors = ColorGenerator::new();
    let pattern_color = colors.next();
    let missing_color = colors.next();
    
    let source = r#"
enum Color {
    Red,
    Green,
    Blue,
    Yellow,
}

fn describe_color(c: Color) -> &'static str {
    match c {
        Color::Red => "red",
        Color::Green => "green",
        // Missing Blue and Yellow
    }
}
"#;
    
    Report::build(ReportKind::Error, 108..149)
        .with_code("E0004")
        .with_message("Non-exhaustive patterns")
        .with_label(
            Label::new(108..149)
                .with_message("Patterns not covered")
                .with_color(pattern_color)
        )
        .with_label(
            Label::new(30..34)
                .with_message(format!("Pattern {} not handled", "Blue".fg(missing_color)))
                .with_color(missing_color)
        )
        .with_label(
            Label::new(40..46)
                .with_message(format!("Pattern {} not handled", "Yellow".fg(missing_color)))
                .with_color(missing_color)
        )
        .with_note("Match arms must cover all possible values")
        .with_help("Add missing patterns:")
        .with_help("    Color::Blue => \"blue\",")
        .with_help("    Color::Yellow => \"yellow\",")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

## Multi-file Examples

### Cross-file Type Error

```rust
use ariadne::{sources, Color, ColorGenerator, Fmt, Label, Report, ReportKind};

fn cross_file_type_error() {
    let mut colors = ColorGenerator::new();
    let def_color = colors.next();
    let use_color = colors.next();
    let error_color = Color::Red;
    
    let cache = sources(vec![
        ("types.rs", r#"
pub struct User {
    pub name: String,
    pub age: u32,
}

impl User {
    pub fn new(name: String, age: u32) -> Self {
        Self { name, age }
    }
}
"#),
        ("main.rs", r#"
mod types;
use types::User;

fn main() {
    let user = User::new("Alice".to_string(), "25");
    println!("User: {} ({})", user.name, user.age);
}
"#),
    ]);
    
    Report::build(ReportKind::Error, ("main.rs", 79..83))
        .with_code("E0308")
        .with_message("Mismatched types in function call")
        .with_label(
            Label::new(("types.rs", 95..98))
                .with_message(format!("Parameter {} expects type {}", "age".fg(def_color), "u32".fg(def_color)))
                .with_color(def_color)
        )
        .with_label(
            Label::new(("main.rs", 79..83))
                .with_message(format!("Argument has type {}", "&str".fg(error_color)))
                .with_color(error_color)
        )
        .with_label(
            Label::new(("main.rs", 49..83))
                .with_message("Function call occurs here")
                .with_color(use_color)
        )
        .with_note(format!("Expected {}, found {}", "u32".fg(def_color), "&str".fg(error_color)))
        .with_help("Convert string to number: `\"25\".parse::<u32>()?`")
        .finish()
        .print(cache)
        .unwrap();
}
```

### Module Resolution Error

```rust
use ariadne::{sources, Color, Label, Report, ReportKind};

fn module_resolution_error() {
    let cache = sources(vec![
        ("main.rs", r#"
mod utils;
mod helpers;

use utils::process_data;
use helpers::format_output;  // Error: module not found

fn main() {
    let data = process_data("hello");
    let output = format_output(data);
    println!("{}", output);
}
"#),
        ("utils.rs", r#"
pub fn process_data(input: &str) -> String {
    input.to_uppercase()
}
"#),
        // Note: helpers.rs doesn't exist
    ]);
    
    Report::build(ReportKind::Error, ("main.rs", 13..20))
        .with_code("E0583")
        .with_message("File not found for module")
        .with_label(
            Label::new(("main.rs", 13..20))
                .with_message("Module declared here")
                .with_color(Color::Red)
        )
        .with_label(
            Label::new(("main.rs", 44..66))
                .with_message("Module used here")
                .with_color(Color::Yellow)
        )
        .with_note("Rust looked for module files:")
        .with_note("  • helpers.rs")
        .with_note("  • helpers/mod.rs")
        .with_help("Create the missing file or remove the module declaration")
        .finish()
        .print(cache)
        .unwrap();
}
```

## Advanced Formatting Examples

### Complex Multi-line Error

```rust
use ariadne::{Color, ColorGenerator, Config, LabelAttach, Label, Report, ReportKind, Source};

fn complex_multiline_error() {
    let mut colors = ColorGenerator::new();
    let expr_color = colors.next();
    let type_color = colors.next();
    let context_color = colors.next();
    
    let config = Config::default()
        .with_label_attach(LabelAttach::Middle)
        .with_multiline_arrows(true)
        .with_compact(false);
    
    let source = r#"
fn calculate_result() -> i32 {
    let x = if true {
        "hello world"
    } else {
        42
    };
    
    x + 10  // Error: cannot add string and integer
}
"#;
    
    Report::build(ReportKind::Error, 94..100)
        .with_code("E0308")
        .with_message("Mismatched types in if expression")
        .with_config(config)
        .with_label(
            Label::new(30..75)
                .with_message(format!("This {} expression has inconsistent types", "if".fg(expr_color)))
                .with_color(expr_color)
                .with_priority(50)
        )
        .with_label(
            Label::new(43..55)
                .with_message(format!("This branch returns {}", "&str".fg(type_color)))
                .with_color(type_color)
                .with_priority(100)
        )
        .with_label(
            Label::new(69..71)
                .with_message(format!("This branch returns {}", "i32".fg(type_color)))
                .with_color(type_color)
                .with_priority(100)
        )
        .with_label(
            Label::new(94..95)
                .with_message(format!("Variable {} used here", "x".fg(context_color)))
                .with_color(context_color)
                .with_priority(75)
        )
        .with_note("All branches of an if expression must return the same type")
        .with_help("Consider converting both branches to the same type")
        .with_help("Or use an enum to represent different value types")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

### Macro Expansion Error

```rust
use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};

fn macro_expansion_error() {
    let mut colors = ColorGenerator::new();
    let macro_color = colors.next();
    let expansion_color = colors.next();
    let error_color = Color::Red;
    
    let source = r#"
macro_rules! create_function {
    ($name:ident, $return_type:ty) => {
        fn $name() -> $return_type {
            "hello"  // Error: wrong return type
        }
    };
}

create_function!(get_number, i32);
"#;
    
    Report::build(ReportKind::Error, 89..96)
        .with_code("E0308")
        .with_message("Mismatched types in macro expansion")
        .with_label(
            Label::new(4..31)
                .with_message(format!("Macro {} defined here", "create_function".fg(macro_color)))
                .with_color(macro_color)
        )
        .with_label(
            Label::new(89..96)
                .with_message(format!("Return value has type {}", "&str".fg(error_color)))
                .with_color(error_color)
        )
        .with_label(
            Label::new(155..158)
                .with_message(format!("Expected return type {}", "i32".fg(expansion_color)))
                .with_color(expansion_color)
        )
        .with_label(
            Label::new(142..171)
                .with_message("Macro invoked here")
                .with_color(expansion_color)
        )
        .with_note("The macro expansion creates a function with mismatched return type")
        .with_help("Change the return value to match the expected type")
        .finish()
        .print(Source::from(source))
        .unwrap();
}
```

## Language Server Examples

### Real-time Diagnostic

```rust
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::collections::HashMap;

struct DiagnosticEngine {
    sources: HashMap<String, Source>,
}

impl DiagnosticEngine {
    fn new() -> Self {
        Self {
            sources: HashMap::new(),
        }
    }
    
    fn update_source(&mut self, uri: String, content: String) -> Vec<Report<(String, std::ops::Range<usize>)>> {
        self.sources.insert(uri.clone(), Source::from(content));
        self.analyze_source(&uri)
    }
    
    fn analyze_source(&self, uri: &str) -> Vec<Report<(String, std::ops::Range<usize>)>> {
        let mut diagnostics = Vec::new();
        
        // Example: detect unused variables
        if let Some(source) = self.sources.get(uri) {
            let text = source.text();
            
            // Simple pattern matching for demonstration
            if let Some(pos) = text.find("let unused") {
                diagnostics.push(
                    Report::build(ReportKind::Warning, (uri.to_string(), pos..pos+10))
                        .with_code("W0001")
                        .with_message("Unused variable")
                        .with_label(
                            Label::new((uri.to_string(), pos..pos+10))
                                .with_message("This variable is never used")
                                .with_color(Color::Yellow)
                        )
                        .with_help("Consider prefixing with underscore")
                        .finish()
                );
            }
        }
        
        diagnostics
    }
}

fn language_server_example() {
    let mut engine = DiagnosticEngine::new();
    
    let diagnostics = engine.update_source(
        "file:///example.rs".to_string(),
        "fn main() { let unused = 42; }".to_string()
    );
    
    // Convert to LSP diagnostics and send to client
    for diagnostic in diagnostics {
        // Convert and send via LSP protocol
        println!("Diagnostic: {:?}", diagnostic);
    }
}
```

### Incremental Analysis

```rust
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::collections::HashMap;

struct IncrementalAnalyzer {
    file_versions: HashMap<String, u32>,
    cached_diagnostics: HashMap<String, Vec<Report<(String, std::ops::Range<usize>)>>>,
}

impl IncrementalAnalyzer {
    fn new() -> Self {
        Self {
            file_versions: HashMap::new(),
            cached_diagnostics: HashMap::new(),
        }
    }
    
    fn analyze_change(&mut self, uri: String, version: u32, content: String) -> &Vec<Report<(String, std::ops::Range<usize>)>> {
        // Check if we need to reanalyze
        let needs_analysis = self.file_versions.get(&uri)
            .map_or(true, |&old_version| old_version != version);
        
        if needs_analysis {
            let diagnostics = self.perform_analysis(&uri, &content);
            self.file_versions.insert(uri.clone(), version);
            self.cached_diagnostics.insert(uri.clone(), diagnostics);
        }
        
        self.cached_diagnostics.get(&uri).unwrap()
    }
    
    fn perform_analysis(&self, uri: &str, content: &str) -> Vec<Report<(String, std::ops::Range<usize>)>> {
        let mut diagnostics = Vec::new();
        
        // Example: check for TODO comments
        for (i, line) in content.lines().enumerate() {
            if let Some(pos) = line.find("TODO") {
                let line_start = content.lines().take(i).map(|l| l.len() + 1).sum::<usize>();
                let span_start = line_start + pos;
                let span_end = span_start + 4;
                
                diagnostics.push(
                    Report::build(ReportKind::Advice, (uri.to_string(), span_start..span_end))
                        .with_code("TODO")
                        .with_message("TODO comment found")
                        .with_label(
                            Label::new((uri.to_string(), span_start..span_end))
                                .with_message("Consider implementing this")
                                .with_color(Color::Blue)
                        )
                        .finish()
                );
            }
        }
        
        diagnostics
    }
}
```

## Performance Examples

### Batch Processing

```rust
use ariadne::{Report, ReportKind, Label, Color, Source};
use std::time::Instant;

struct BatchProcessor {
    reports: Vec<Report<std::ops::Range<usize>>>,
    max_reports: usize,
}

impl BatchProcessor {
    fn new(max_reports: usize) -> Self {
        Self {
            reports: Vec::with_capacity(max_reports),
            max_reports,
        }
    }
    
    fn add_error(&mut self, span: std::ops::Range<usize>, message: String) {
        if self.reports.len() < self.max_reports {
            self.reports.push(
                Report::build(ReportKind::Error, span)
                    .with_message(message)
                    .with_label(
                        Label::new(span)
                            .with_color(Color::Red)
                    )
                    .finish()
            );
        }
    }
    
    fn flush(&mut self, source: &Source) -> std::io::Result<()> {
        let start = Instant::now();
        
        // Sort by position for better output
        self.reports.sort_by_key(|r| r.span.start);
        
        for report in &self.reports {
            report.print(source)?;
        }
        
        let duration = start.elapsed();
        eprintln!("Printed {} reports in {:?}", self.reports.len(), duration);
        
        self.reports.clear();
        Ok(())
    }
}

fn batch_processing_example() {
    let mut processor = BatchProcessor::new(100);
    let source = Source::from("let x = 1; let y = 2; let z = 3;");
    
    // Add many errors
    for i in 0..50 {
        processor.add_error(
            i*2..i*2+1,
            format!("Error #{}", i)
        );
    }
    
    // Flush all at once
    processor.flush(&source).unwrap();
}
```

### Memory-Efficient Streaming

```rust
use ariadne::{Report, ReportKind, Label, Color, Source};
use std::io::Write;

struct StreamingReporter<W: Write> {
    writer: W,
    report_count: usize,
}

impl<W: Write> StreamingReporter<W> {
    fn new(writer: W) -> Self {
        Self {
            writer,
            report_count: 0,
        }
    }
    
    fn report_error(&mut self, source: &Source, span: std::ops::Range<usize>, message: String) -> std::io::Result<()> {
        let report = Report::build(ReportKind::Error, span.clone())
            .with_message(message)
            .with_label(
                Label::new(span)
                    .with_color(Color::Red)
            )
            .finish();
        
        // Write immediately, don't store
        report.write(source, &mut self.writer)?;
        self.report_count += 1;
        
        Ok(())
    }
    
    fn report_count(&self) -> usize {
        self.report_count
    }
}

fn streaming_example() {
    let source = Source::from("let x = 1; let y = 2; let z = 3;");
    let mut reporter = StreamingReporter::new(std::io::stderr());
    
    // Reports are written immediately, using minimal memory
    for i in 0..1000 {
        reporter.report_error(
            &source,
            i%20..i%20+1,
            format!("Streaming error #{}", i)
        ).unwrap();
    }
    
    println!("Streamed {} reports", reporter.report_count());
}
```

These examples demonstrate the full range of Ariadne's capabilities, from simple error reporting to complex multi-file diagnostics and performance-optimized batch processing. Each example is self-contained and can be adapted for specific use cases.