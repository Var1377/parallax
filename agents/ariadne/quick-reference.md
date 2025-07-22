# Ariadne Quick Reference

A concise reference for common Ariadne operations and patterns.

## Basic Usage

### Simple Error Report
```rust
use ariadne::{Color, Label, Report, ReportKind, Source};

Report::build(ReportKind::Error, 10..15)
    .with_message("Error message")
    .with_label(Label::new(10..15).with_color(Color::Red))
    .finish()
    .print(Source::from("source code"))
    .unwrap();
```

### Multi-label Report
```rust
Report::build(ReportKind::Error, 10..15)
    .with_code("E001")
    .with_message("Type mismatch")
    .with_label(Label::new(5..8).with_message("Expected i32").with_color(Color::Blue))
    .with_label(Label::new(10..15).with_message("Found String").with_color(Color::Red))
    .with_note("Types must match")
    .with_help("Try converting: value.parse::<i32>()?")
    .finish()
```

## Report Types

| Type | Usage | Color |
|------|-------|-------|
| `ReportKind::Error` | Critical errors | Red |
| `ReportKind::Warning` | Potential issues | Yellow |
| `ReportKind::Advice` | Suggestions | Blue |
| `ReportKind::Custom("Info", Color::Green)` | Custom type | Custom |

## Span Types

```rust
// Simple range
Label::new(10..15)

// Inclusive range  
Label::new(10..=14)

// With file ID
Label::new(("file.rs", 10..15))

// Custom span (implement Span trait)
struct MySpan { file: String, start: usize, end: usize }
```

## Colors

### Basic Colors
```rust
Color::Red, Color::Green, Color::Blue, Color::Yellow
Color::Magenta, Color::Cyan, Color::White, Color::Black
```

### Advanced Colors
```rust
Color::Fixed(196)           // 8-bit color (0-255)
Color::Rgb(255, 128, 0)     // 24-bit RGB
```

### Color Generator
```rust
let mut colors = ColorGenerator::new();
let color1 = colors.next();  // Automatically distinct colors
let color2 = colors.next();
```

## Text Formatting

```rust
use ariadne::Fmt;

format!("Error in {}", "function".fg(Color::Red))
format!("Bold {}", "text".bold())
format!("Underlined {}", "text".underline())
format!("Combined {}", "text".fg(Color::Blue).bold())
```

## Source Management

### Single Source
```rust
let source = Source::from("code content");
report.print(source).unwrap();
```

### Multi-file Cache
```rust
use ariadne::sources;

let cache = sources(vec![
    ("main.rs", "fn main() {}"),
    ("lib.rs", "pub fn hello() {}"),
]);
report.print(cache).unwrap();
```

### File System Cache
```rust
use ariadne::FileCache;

let mut cache = FileCache::default();
report.print(&mut cache).unwrap();  // Loads files automatically
```

### Custom Cache
```rust
use ariadne::FnCache;

let cache = FnCache::new(|path: &str| {
    std::fs::read_to_string(path)  // Custom loading logic
});
```

## Configuration

```rust
use ariadne::{Config, CharSet, LabelAttach, IndexType};

let config = Config::default()
    .with_color(true)                    // Enable colors
    .with_compact(false)                 // Full spacing
    .with_char_set(CharSet::Unicode)     // Unicode or Ascii
    .with_label_attach(LabelAttach::Middle) // Start, Middle, End
    .with_tab_width(4)                   // Tab character width
    .with_underlines(true)               // Show underlines
    .with_multiline_arrows(true)         // Show arrows for multiline
    .with_cross_gap(true)                // Gaps at line crossings
    .with_index_type(IndexType::Char);   // Char or Byte indexing

Report::build(ReportKind::Error, span)
    .with_config(config)
    .finish()
```

## Label Properties

```rust
Label::new(span)
    .with_message("Label text")
    .with_color(Color::Red)
    .with_priority(100)      // Higher = more important
    .with_order(-1)          // Lower = shown first
```

## Output Methods

```rust
// Print to stdout
report.print(cache).unwrap();

// Print to stderr  
report.eprint(cache).unwrap();

// Write to custom destination
let mut buffer = Vec::new();
report.write(cache, &mut buffer).unwrap();

// Write with custom config
report.write_with_config(cache, &mut buffer, &config).unwrap();
```

## Common Patterns

### Type Error
```rust
let mut colors = ColorGenerator::new();
let expected = colors.next();
let found = colors.next();

Report::build(ReportKind::Error, span)
    .with_message("Type mismatch")
    .with_label(Label::new(expected_span)
        .with_message(format!("Expected {}", "i32".fg(expected)))
        .with_color(expected))
    .with_label(Label::new(found_span)
        .with_message(format!("Found {}", "String".fg(found)))
        .with_color(found))
    .finish()
```

### Undefined Variable
```rust
Report::build(ReportKind::Error, span)
    .with_code("E0425")
    .with_message(format!("Cannot find value `{}` in scope", name))
    .with_label(Label::new(span)
        .with_message("Not found")
        .with_color(Color::Red))
    .with_help("Did you mean `similar_name`?")
    .finish()
```

### Warning with Suggestion
```rust
Report::build(ReportKind::Warning, span)
    .with_message("Unused variable")
    .with_label(Label::new(span)
        .with_message("Never used")
        .with_color(Color::Yellow))
    .with_help("Prefix with underscore: `_variable`")
    .finish()
```

### Multi-file Error
```rust
Report::build(ReportKind::Error, ("file2.rs", error_span))
    .with_message("Function not found")
    .with_label(Label::new(("file2.rs", error_span))
        .with_message("Called here")
        .with_color(Color::Red))
    .with_label(Label::new(("file1.rs", def_span))
        .with_message("Similar function defined here")
        .with_color(Color::Blue))
    .finish()
```

## Performance Tips

### Batch Processing
```rust
let mut reports = Vec::new();
// Collect reports...

// Sort and print in batch
reports.sort_by_key(|r| r.span.start());
for report in reports {
    report.print(&cache)?;
}
```

### Streaming
```rust
// For large numbers of reports
let mut writer = std::io::stderr();
for report in reports {
    report.write(&cache, &mut writer)?;
}
```

### Memory Optimization
```rust
let config = Config::default()
    .with_compact(true)
    .with_index_type(IndexType::Byte)  // O(1) lookups
    .with_color(false);                // Reduce memory
```

## Error Handling

```rust
// Handle write errors
match report.print(&cache) {
    Ok(()) => {},
    Err(e) => eprintln!("Failed to print report: {}", e),
}

// Validate spans
if span.start() <= span.end() && span.end() <= source.len() {
    // Safe to use span
}

// Limit error count
let mut error_count = 0;
const MAX_ERRORS: usize = 10;

if error_count < MAX_ERRORS {
    emit_report(report);
    error_count += 1;
} else {
    eprintln!("Too many errors, stopping...");
}
```

## Testing

### Snapshot Testing
```rust
#[cfg(test)]
fn test_error_output() {
    let report = create_test_report();
    let source = Source::from("test code");
    
    let mut buffer = Vec::new();
    report.write(&source, &mut buffer).unwrap();
    let output = String::from_utf8(buffer).unwrap();
    
    insta::assert_snapshot!(output);
}
```

### Property Testing
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn reports_never_panic(start: usize, end: usize, text: String) {
        let start = start.min(text.len());
        let end = end.max(start).min(text.len());
        
        let report = Report::build(ReportKind::Error, start..end)
            .finish();
        let source = Source::from(text);
        
        // Should never panic
        let _ = report.write(&source, &mut Vec::new());
    }
}
```

## Integration Examples

### Language Server
```rust
fn to_lsp_diagnostic(report: &Report<Span>) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic {
        range: span_to_lsp_range(&report.span),
        severity: match report.kind {
            ReportKind::Error => Some(DiagnosticSeverity::ERROR),
            ReportKind::Warning => Some(DiagnosticSeverity::WARNING),
            _ => Some(DiagnosticSeverity::INFORMATION),
        },
        message: report.msg.clone().unwrap_or_default(),
        ..Default::default()
    }
}
```

### Build System
```rust
fn main() -> ExitCode {
    let mut error_count = 0;
    let mut cache = FileCache::default();
    
    for report in analyze_project() {
        if matches!(report.kind, ReportKind::Error) {
            error_count += 1;
        }
        report.eprint(&mut cache).unwrap();
    }
    
    if error_count > 0 {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
```

This quick reference covers the most common Ariadne usage patterns. For detailed examples and advanced features, see the full documentation.