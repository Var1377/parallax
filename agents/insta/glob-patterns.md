# Glob Patterns for Multiple File Testing

The glob feature in Insta allows you to test multiple files efficiently using pattern matching. This is particularly useful for testing parsers, compilers, template engines, or any system that processes multiple input files.

## Enabling Glob Support

Add the glob feature to your `Cargo.toml`:

```toml
[dev-dependencies]
insta = { version = "1.43", features = ["glob"] }
```

## Basic Glob Usage

### Simple File Pattern

```rust
use insta::glob;

#[test]
fn test_text_files() {
    glob!("inputs/*.txt", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        insta::assert_snapshot!(content);
    });
}
```

This will:
1. Find all `.txt` files in the `inputs/` directory
2. Run the closure for each file
3. Create individual snapshots for each file

### Directory Structure Example

```
tests/
├── inputs/
│   ├── hello.txt
│   ├── world.txt
│   └── greeting.txt
├── snapshots/
│   ├── test_text_files@hello.txt.snap
│   ├── test_text_files@world.txt.snap
│   └── test_text_files@greeting.txt.snap
└── glob_test.rs
```

## Glob Pattern Syntax

### Wildcards

```rust
// Single character wildcard
glob!("inputs/test?.txt", |path| { /* ... */ });

// Multiple character wildcard
glob!("inputs/*.txt", |path| { /* ... */ });

// Recursive wildcard (any depth)
glob!("inputs/**/*.txt", |path| { /* ... */ });
```

### Character Classes

```rust
// Match specific characters
glob!("inputs/test[123].txt", |path| { /* ... */ });

// Match character ranges
glob!("inputs/test[a-z].txt", |path| { /* ... */ });

// Negate character class
glob!("inputs/test[!0-9].txt", |path| { /* ... */ });
```

### Multiple Extensions

```rust
// Match multiple file types
glob!("inputs/*.{txt,md,json}", |path| {
    let content = std::fs::read_to_string(path).unwrap();
    insta::assert_snapshot!(content);
});
```

## Advanced Glob Examples

### Testing a Parser

```rust
use insta::glob;

#[test]
fn test_parser_inputs() {
    glob!("parser_tests/**/*.input", |path| {
        let input = std::fs::read_to_string(path).unwrap();
        
        match parse_expression(&input) {
            Ok(ast) => {
                insta::assert_debug_snapshot!(ast);
            }
            Err(error) => {
                insta::assert_snapshot!(format!("Parse error: {}", error));
            }
        }
    });
}

fn parse_expression(input: &str) -> Result<Ast, ParseError> {
    // Your parser implementation
    todo!()
}

#[derive(Debug)]
struct Ast {
    // Your AST structure
}

#[derive(Debug)]
struct ParseError {
    // Your error type
}
```

### Testing Template Engine

```rust
use insta::glob;
use serde_json::Value;

#[test]
fn test_template_rendering() {
    glob!("templates/**/*.hbs", |path| {
        let template_content = std::fs::read_to_string(path).unwrap();
        
        // Look for corresponding data file
        let data_path = path.with_extension("json");
        let data: Value = if data_path.exists() {
            let data_content = std::fs::read_to_string(data_path).unwrap();
            serde_json::from_str(&data_content).unwrap()
        } else {
            serde_json::json!({})
        };
        
        let rendered = render_template(&template_content, &data).unwrap();
        insta::assert_snapshot!(rendered);
    });
}

fn render_template(template: &str, data: &Value) -> Result<String, TemplateError> {
    // Your template engine implementation
    todo!()
}

#[derive(Debug)]
struct TemplateError;
```

### Testing Compiler Output

```rust
use insta::glob;

#[test]
fn test_compiler_outputs() {
    glob!("compiler_tests/**/*.src", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        
        let compilation_result = compile_source(&source);
        
        match compilation_result {
            Ok(bytecode) => {
                insta::assert_debug_snapshot!("bytecode", bytecode);
            }
            Err(errors) => {
                let error_summary: Vec<String> = errors
                    .iter()
                    .map(|e| format!("{}:{}: {}", e.line, e.column, e.message))
                    .collect();
                insta::assert_yaml_snapshot!("errors", error_summary);
            }
        }
    });
}

#[derive(Debug)]
struct Bytecode {
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
struct Instruction {
    opcode: String,
    operands: Vec<i32>,
}

#[derive(Debug)]
struct CompileError {
    line: usize,
    column: usize,
    message: String,
}

fn compile_source(source: &str) -> Result<Bytecode, Vec<CompileError>> {
    // Your compiler implementation
    todo!()
}
```

## Glob with Different Formats

### JSON Processing

```rust
use insta::glob;
use serde_json::Value;

#[test]
fn test_json_transformations() {
    glob!("json_inputs/*.json", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        let input: Value = serde_json::from_str(&content).unwrap();
        
        let transformed = transform_json(input);
        insta::assert_json_snapshot!(transformed);
    });
}

fn transform_json(input: Value) -> Value {
    // Your JSON transformation logic
    match input {
        Value::Object(mut obj) => {
            // Add metadata
            obj.insert("processed_at".to_string(), 
                      Value::String("2023-01-15T10:30:00Z".to_string()));
            obj.insert("version".to_string(), Value::String("1.0".to_string()));
            Value::Object(obj)
        }
        other => other,
    }
}
```

### YAML Configuration Testing

```rust
use insta::glob;
use serde_yaml::Value;

#[test]
fn test_config_validation() {
    glob!("configs/**/*.yml", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        let config: Value = serde_yaml::from_str(&content).unwrap();
        
        let validation_result = validate_config(&config);
        insta::assert_yaml_snapshot!(validation_result);
    });
}

#[derive(serde::Serialize)]
struct ValidationResult {
    valid: bool,
    errors: Vec<String>,
    warnings: Vec<String>,
}

fn validate_config(config: &Value) -> ValidationResult {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    
    // Your validation logic
    if !config.get("name").is_some() {
        errors.push("Missing required field: name".to_string());
    }
    
    if !config.get("version").is_some() {
        warnings.push("Version field not specified".to_string());
    }
    
    ValidationResult {
        valid: errors.is_empty(),
        errors,
        warnings,
    }
}
```

## Glob with Custom Base Paths

### Three-Argument Form

```rust
use insta::glob;
use std::path::Path;

#[test]
fn test_with_custom_base() {
    let base_path = Path::new("custom_test_data");
    
    glob!(base_path, "**/*.test", |path| {
        let relative_path = path.strip_prefix(base_path).unwrap();
        let content = std::fs::read_to_string(path).unwrap();
        
        // Process with knowledge of the relative path
        let result = process_test_file(relative_path, &content);
        insta::assert_snapshot!(result);
    });
}

fn process_test_file(relative_path: &Path, content: &str) -> String {
    format!("Processing {}: {}", relative_path.display(), content.trim())
}
```

### Nested Directory Testing

```rust
#[test]
fn test_nested_directories() {
    glob!("test_cases/**/input.txt", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        
        // Extract test case name from path
        let test_case = path.parent()
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");
            
        let result = process_test_case(test_case, &content);
        insta::assert_snapshot!(result);
    });
}

fn process_test_case(name: &str, input: &str) -> String {
    format!("Test case '{}' result: {}", name, input.to_uppercase())
}
```

## Environment Variables and Configuration

### INSTA_GLOB_FILTER

Filter which files are processed:

```bash
# Only process files matching the filter
INSTA_GLOB_FILTER="*.rs;*.toml" cargo test

# Multiple filters separated by semicolons
INSTA_GLOB_FILTER="test_*.txt;example_*.json" cargo test
```

### Allow Empty Globs

```rust
use insta::Settings;

#[test]
fn test_optional_files() {
    let mut settings = Settings::clone_current();
    settings.set_allow_empty_glob(true);
    
    settings.bind(|| {
        // This won't panic if no files match
        glob!("optional_inputs/*.txt", |path| {
            let content = std::fs::read_to_string(path).unwrap();
            insta::assert_snapshot!(content);
        });
    });
}
```

## Error Handling in Glob Tests

### Graceful Error Handling

```rust
#[test]
fn test_with_error_handling() {
    glob!("inputs/*.json", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        
        match serde_json::from_str::<serde_json::Value>(&content) {
            Ok(json) => {
                insta::assert_json_snapshot!("valid", json);
            }
            Err(error) => {
                let error_info = serde_json::json!({
                    "file": path.file_name().unwrap().to_string_lossy(),
                    "error": error.to_string(),
                    "line": error.line(),
                    "column": error.column()
                });
                insta::assert_json_snapshot!("error", error_info);
            }
        }
    });
}
```

### Collecting Results

```rust
#[test]
fn test_batch_processing() {
    let mut results = Vec::new();
    
    glob!("batch_inputs/*.txt", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        let filename = path.file_name().unwrap().to_string_lossy();
        
        let processed = process_content(&content);
        results.push(serde_json::json!({
            "file": filename,
            "lines": content.lines().count(),
            "processed": processed
        }));
    });
    
    // Snapshot the collected results
    insta::assert_json_snapshot!(results);
}

fn process_content(content: &str) -> String {
    content.to_uppercase()
}
```

## Performance Considerations

### Large File Sets

```rust
#[test]
fn test_large_file_set() {
    let mut file_count = 0;
    
    glob!("large_dataset/**/*.data", |path| {
        file_count += 1;
        
        // Process only a sample for performance
        if file_count <= 10 {
            let content = std::fs::read_to_string(path).unwrap();
            let summary = summarize_content(&content);
            insta::assert_snapshot!(summary);
        }
    });
    
    // Snapshot the total count
    insta::assert_snapshot!(format!("Total files processed: {}", file_count));
}

fn summarize_content(content: &str) -> String {
    format!("Length: {}, Lines: {}", content.len(), content.lines().count())
}
```

### Parallel Processing Considerations

```rust
// Note: Glob tests run sequentially by default
// Each file creates its own snapshot, so they don't interfere
#[test]
fn test_independent_files() {
    glob!("independent/**/*.txt", |path| {
        // Each file is processed independently
        let content = std::fs::read_to_string(path).unwrap();
        let result = expensive_processing(&content);
        insta::assert_snapshot!(result);
    });
}

fn expensive_processing(content: &str) -> String {
    // Simulate expensive processing
    std::thread::sleep(std::time::Duration::from_millis(10));
    content.to_uppercase()
}
```

## Best Practices

### 1. Organize Test Files Clearly

```
tests/
├── parser_tests/
│   ├── valid/
│   │   ├── simple.expr
│   │   ├── complex.expr
│   │   └── nested.expr
│   ├── invalid/
│   │   ├── syntax_error.expr
│   │   └── type_error.expr
│   └── snapshots/
└── glob_parser_test.rs
```

### 2. Use Descriptive File Names

```rust
// ❌ Generic names
// test1.txt, test2.txt, test3.txt

// ✅ Descriptive names
// empty_input.txt, single_line.txt, multiline_with_quotes.txt
```

### 3. Handle Edge Cases

```rust
#[test]
fn test_edge_cases() {
    glob!("edge_cases/*.txt", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        
        let result = if content.is_empty() {
            "empty_file".to_string()
        } else if content.len() > 10000 {
            format!("large_file_{}_bytes", content.len())
        } else {
            process_normal_file(&content)
        };
        
        insta::assert_snapshot!(result);
    });
}

fn process_normal_file(content: &str) -> String {
    content.trim().to_string()
}
```

### 4. Use Consistent Snapshot Names

```rust
// Snapshot names will be based on the file names
// Make sure your input files have clear, consistent names
// that will result in readable snapshot names

// Input: parser_tests/expressions/arithmetic_add.expr
// Snapshot: test_parser@arithmetic_add.expr.snap
```

### 5. Document Your Test Structure

```rust
/// Tests the expression parser against various input files.
/// 
/// Test files are organized as:
/// - `valid/` - Files that should parse successfully
/// - `invalid/` - Files that should produce parse errors
/// - `edge_cases/` - Boundary conditions and special cases
/// 
/// Each file produces a snapshot showing either the parsed AST
/// or the error message for invalid inputs.
#[test]
fn test_expression_parser() {
    glob!("parser_tests/**/*.expr", |path| {
        // Implementation...
    });
}
```

## Troubleshooting

### No Files Matched

```rust
// Check if your pattern is correct
#[test]
fn debug_glob_pattern() {
    use std::path::Path;
    
    let pattern = "inputs/*.txt";
    let base = Path::new(".");
    
    // Manually check what files exist
    if let Ok(entries) = std::fs::read_dir("inputs") {
        for entry in entries {
            if let Ok(entry) = entry {
                println!("Found file: {:?}", entry.path());
            }
        }
    }
    
    // Then run the glob
    glob!(pattern, |path| {
        println!("Processing: {:?}", path);
        insta::assert_snapshot!("found_file", path.display().to_string());
    });
}
```

### Path Issues

```rust
// Use absolute paths if relative paths don't work
#[test]
fn test_with_absolute_path() {
    let current_dir = std::env::current_dir().unwrap();
    let test_dir = current_dir.join("tests").join("inputs");
    
    glob!(test_dir, "*.txt", |path| {
        let content = std::fs::read_to_string(path).unwrap();
        insta::assert_snapshot!(content);
    });
}
```

### Parent Directory Traversal Error

```rust
// ❌ This will panic
glob!("../other_tests/*.txt", |path| { /* ... */ });

// ✅ Use explicit base path instead
glob!(Path::new("../other_tests"), "*.txt", |path| { /* ... */ });
```