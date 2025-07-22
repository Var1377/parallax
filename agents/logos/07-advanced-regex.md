# Advanced Regex Patterns

This chapter covers advanced regular expression patterns in Logos, including complex matching scenarios, Unicode support, and performance considerations.

## Regex Fundamentals in Logos

### Supported Regex Features

Logos uses `regex-syntax` for pattern parsing and supports most standard regex features:

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    // Character classes
    #[regex("[a-zA-Z]")]
    Letter,
    
    #[regex("[0-9]")]
    Digit,
    
    #[regex(r"[\w]")]
    WordChar,
    
    #[regex(r"[\s]")]
    Whitespace,
    
    // Quantifiers
    #[regex("[a-zA-Z]+")]          // One or more
    Word,
    
    #[regex("[0-9]*")]             // Zero or more
    OptionalNumber,
    
    #[regex("[a-zA-Z]?")]          // Zero or one
    OptionalLetter,
    
    #[regex("[0-9]{3}")]           // Exactly 3
    ThreeDigits,
    
    #[regex("[0-9]{2,4}")]         // Between 2 and 4
    TwoToFourDigits,
    
    #[regex("[0-9]{3,}")]          // 3 or more
    ThreeOrMoreDigits,
    
    // Alternation
    #[regex("cat|dog|bird")]
    Animal,
    
    // Grouping
    #[regex("(red|blue|green)_(car|bike)")]
    ColoredVehicle,
}
```

### Unsupported Features

Logos doesn't support features that require backtracking:

```rust
// ❌ Not supported - non-greedy quantifiers
// #[regex(".*?")]
// #[regex(".+?")]

// ❌ Not supported - word boundaries
// #[regex(r"\bword\b")]

// ❌ Not supported - anchors
// #[regex("^start")]
// #[regex("end$")]

// ❌ Not supported - lookahead/lookbehind
// #[regex("(?=pattern)")]
// #[regex("(?<=pattern)")]

// ❌ Not supported - backreferences
// #[regex(r"(\w+)\s+\1")]
```

## Complex Pattern Examples

### Programming Language Tokens

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//[^\n]*")]                    // Line comments
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]       // Block comments
enum ProgrammingToken {
    // Identifiers and keywords
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
    
    // Numbers
    #[regex(r"[0-9]+")]
    Integer,
    
    #[regex(r"[0-9]+\.[0-9]+")]
    Float,
    
    #[regex(r"[0-9]+[eE][+-]?[0-9]+")]
    Scientific,
    
    #[regex(r"0[xX][0-9a-fA-F]+")]
    Hexadecimal,
    
    #[regex(r"0[bB][01]+")]
    Binary,
    
    #[regex(r"0[oO][0-7]+")]
    Octal,
    
    // Strings
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    
    #[regex(r"'([^'\\]|\\.)*'")]
    CharLiteral,
    
    // Raw strings (Rust-style)
    #[regex(r#"r"[^"]*""#)]
    RawString,
    
    #[regex(r#"r#"[^"]*"#"#)]
    RawStringHash,
    
    // Operators
    #[regex(r"[+\-*/%=<>!&|^~]")]
    Operator,
    
    #[regex(r"(==|!=|<=|>=|&&|\|\||<<|>>|\+=|-=|\*=|/=|%=)")]
    CompoundOperator,
}
```

### URL and Email Patterns

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum WebToken {
    // Email addresses (simplified)
    #[regex(r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}")]
    Email,
    
    // URLs
    #[regex(r"https?://[a-zA-Z0-9.-]+(/[a-zA-Z0-9./_-]*)?")]
    Url,
    
    // IP addresses
    #[regex(r"([0-9]{1,3}\.){3}[0-9]{1,3}")]
    IpAddress,
    
    // Domain names
    #[regex(r"[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}")]
    Domain,
    
    // Phone numbers (US format)
    #[regex(r"\([0-9]{3}\)\s?[0-9]{3}-[0-9]{4}")]
    PhoneNumber,
    
    // Credit card numbers (simplified)
    #[regex(r"[0-9]{4}(-[0-9]{4}){3}")]
    CreditCard,
}
```

### Date and Time Patterns

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum DateTimeToken {
    // ISO 8601 date
    #[regex(r"[0-9]{4}-[0-9]{2}-[0-9]{2}")]
    IsoDate,
    
    // ISO 8601 datetime
    #[regex(r"[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}")]
    IsoDateTime,
    
    // US date format
    #[regex(r"[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}")]
    UsDate,
    
    // Time (24-hour)
    #[regex(r"[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?")]
    Time24,
    
    // Time (12-hour with AM/PM)
    #[regex(r"[0-9]{1,2}:[0-9]{2}\s?(AM|PM)")]
    Time12,
    
    // Unix timestamp
    #[regex(r"[0-9]{10}")]
    UnixTimestamp,
}
```

## Unicode Support

### Unicode Character Classes

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum UnicodeToken {
    // Unicode letters
    #[regex(r"[\p{L}]+")]
    Letters,
    
    // Unicode numbers
    #[regex(r"[\p{N}]+")]
    Numbers,
    
    // Unicode punctuation
    #[regex(r"[\p{P}]+")]
    Punctuation,
    
    // Unicode symbols
    #[regex(r"[\p{S}]+")]
    Symbols,
    
    // Specific Unicode blocks
    #[regex(r"[\p{Greek}]+")]
    Greek,
    
    #[regex(r"[\p{Cyrillic}]+")]
    Cyrillic,
    
    #[regex(r"[\p{Han}]+")]
    Chinese,
    
    #[regex(r"[\p{Hiragana}\p{Katakana}]+")]
    Japanese,
    
    // Emoji
    #[regex(r"[\p{Emoji}]+")]
    Emoji,
    
    // Combining characters
    #[regex(r"[\p{M}]+")]
    CombiningMarks,
}
```

### International Identifiers

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum InternationalToken {
    // Unicode identifier (following Unicode Standard Annex #31)
    #[regex(r"[\p{XID_Start}][\p{XID_Continue}]*")]
    Identifier,
    
    // ASCII-only identifier
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    AsciiIdentifier,
    
    // Mixed script detection
    #[regex(r"[\p{Latin}]+")]
    Latin,
    
    #[regex(r"[\p{Arabic}]+")]
    Arabic,
    
    #[regex(r"[\p{Hebrew}]+")]
    Hebrew,
    
    // Numbers in different scripts
    #[regex(r"[\p{Nd}]+")]
    DecimalNumbers,
    
    #[regex(r"[\p{Nl}]+")]
    LetterNumbers,
    
    #[regex(r"[\p{No}]+")]
    OtherNumbers,
}
```

## Advanced String Processing

### Complex String Literals

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum StringToken {
    // Basic string with escape sequences
    #[regex(r#""([^"\\]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#, parse_string)]
    String(String),
    
    // Raw string (no escape processing)
    #[regex(r#"r"([^"]*)""#, parse_raw_string)]
    RawString(String),
    
    // Multi-line string
    #[regex(r#"```([^`]|`[^`]|``[^`])*```"#, parse_multiline_string)]
    MultilineString(String),
    
    // Template string with interpolation
    #[regex(r#"`([^`\\$]|\\.|$\{[^}]*\})*`"#, parse_template_string)]
    TemplateString(Vec<StringPart>),
}

#[derive(Debug, PartialEq)]
enum StringPart {
    Text(String),
    Interpolation(String),
}

fn parse_string(lex: &mut logos::Lexer<StringToken>) -> Option<String> {
    let slice = lex.slice();
    let content = &slice[1..slice.len()-1]; // Remove quotes
    
    let mut result = String::new();
    let mut chars = content.chars();
    
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next()? {
                '"' => result.push('"'),
                '\\' => result.push('\\'),
                'b' => result.push('\u{0008}'),
                'n' => result.push('\n'),
                'f' => result.push('\u{000C}'),
                'r' => result.push('\r'),
                't' => result.push('\t'),
                '/' => result.push('/'),
                'u' => {
                    // Unicode escape: \uXXXX
                    let mut hex = String::new();
                    for _ in 0..4 {
                        hex.push(chars.next()?);
                    }
                    if let Ok(code) = u32::from_str_radix(&hex, 16) {
                        if let Some(unicode_char) = char::from_u32(code) {
                            result.push(unicode_char);
                        }
                    }
                }
                c => result.push(c),
            }
        } else {
            result.push(ch);
        }
    }
    
    Some(result)
}

fn parse_raw_string(lex: &mut logos::Lexer<StringToken>) -> Option<String> {
    let slice = lex.slice();
    // Remove r" and "
    Some(slice[2..slice.len()-1].to_string())
}

fn parse_multiline_string(lex: &mut logos::Lexer<StringToken>) -> Option<String> {
    let slice = lex.slice();
    // Remove ``` and ```
    Some(slice[3..slice.len()-3].to_string())
}

fn parse_template_string(lex: &mut logos::Lexer<StringToken>) -> Option<Vec<StringPart>> {
    let slice = lex.slice();
    let content = &slice[1..slice.len()-1]; // Remove backticks
    
    let mut parts = Vec::new();
    let mut current_text = String::new();
    let mut chars = content.chars().peekable();
    
    while let Some(ch) = chars.next() {
        if ch == '$' && chars.peek() == Some(&'{') {
            // Start of interpolation
            if !current_text.is_empty() {
                parts.push(StringPart::Text(current_text.clone()));
                current_text.clear();
            }
            
            chars.next(); // consume '{'
            let mut interpolation = String::new();
            let mut brace_count = 1;
            
            while let Some(ch) = chars.next() {
                if ch == '{' {
                    brace_count += 1;
                } else if ch == '}' {
                    brace_count -= 1;
                    if brace_count == 0 {
                        break;
                    }
                }
                interpolation.push(ch);
            }
            
            parts.push(StringPart::Interpolation(interpolation));
        } else if ch == '\\' {
            // Handle escape sequences
            if let Some(escaped) = chars.next() {
                match escaped {
                    'n' => current_text.push('\n'),
                    't' => current_text.push('\t'),
                    'r' => current_text.push('\r'),
                    '\\' => current_text.push('\\'),
                    '`' => current_text.push('`'),
                    '$' => current_text.push('$'),
                    c => {
                        current_text.push('\\');
                        current_text.push(c);
                    }
                }
            }
        } else {
            current_text.push(ch);
        }
    }
    
    if !current_text.is_empty() {
        parts.push(StringPart::Text(current_text));
    }
    
    Some(parts)
}
```

### Regular Expression Validation

```rust
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum ValidationToken {
    // Email validation (more comprehensive)
    #[regex(r"[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*", validate_email)]
    Email(String),
    
    // URL validation
    #[regex(r"https?://(?:[-\w.])+(?:\:[0-9]+)?(?:/(?:[\w/_.])*(?:\?(?:[\w&=%.])*)?(?:#(?:[\w.])*)?)?", validate_url)]
    Url(String),
    
    // IPv4 address validation
    #[regex(r"(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)", validate_ipv4)]
    Ipv4(String),
    
    // Credit card validation (Luhn algorithm)
    #[regex(r"[0-9]{13,19}", validate_credit_card)]
    CreditCard(String),
}

fn validate_email(lex: &mut logos::Lexer<ValidationToken>) -> Option<String> {
    let email = lex.slice();
    
    // Additional validation beyond regex
    if email.len() > 254 {
        return None; // Email too long
    }
    
    let parts: Vec<&str> = email.split('@').collect();
    if parts.len() != 2 {
        return None;
    }
    
    let (local, domain) = (parts[0], parts[1]);
    
    if local.len() > 64 || domain.len() > 253 {
        return None;
    }
    
    // Check for consecutive dots
    if email.contains("..") {
        return None;
    }
    
    Some(email.to_string())
}

fn validate_url(lex: &mut logos::Lexer<ValidationToken>) -> Option<String> {
    let url = lex.slice();
    
    // Additional URL validation
    if url.len() > 2048 {
        return None; // URL too long
    }
    
    // Check for valid characters in path
    if url.contains(' ') {
        return None;
    }
    
    Some(url.to_string())
}

fn validate_ipv4(lex: &mut logos::Lexer<ValidationToken>) -> Option<String> {
    let ip = lex.slice();
    
    // Validate each octet
    for octet in ip.split('.') {
        if let Ok(num) = octet.parse::<u8>() {
            // Leading zeros not allowed (except for "0")
            if octet.len() > 1 && octet.starts_with('0') {
                return None;
            }
        } else {
            return None;
        }
    }
    
    Some(ip.to_string())
}

fn validate_credit_card(lex: &mut logos::Lexer<ValidationToken>) -> Option<String> {
    let number = lex.slice();
    
    // Luhn algorithm validation
    let digits: Vec<u32> = number.chars()
        .filter_map(|c| c.to_digit(10))
        .collect();
    
    if digits.is_empty() {
        return None;
    }
    
    let mut sum = 0;
    let mut alternate = false;
    
    for &digit in digits.iter().rev() {
        let mut n = digit;
        if alternate {
            n *= 2;
            if n > 9 {
                n = (n / 10) + (n % 10);
            }
        }
        sum += n;
        alternate = !alternate;
    }
    
    if sum % 10 == 0 {
        Some(number.to_string())
    } else {
        None
    }
}
```

## Performance Optimization for Regex

### Efficient Character Classes

```rust
#[derive(Logos, Debug, PartialEq)]
enum OptimizedToken {
    // ✅ Fast - specific ranges
    #[regex("[a-zA-Z0-9_]")]
    Identifier,
    
    // ❌ Slower - Unicode classes
    #[regex(r"[\p{L}\p{N}_]")]
    UnicodeIdentifier,
    
    // ✅ Fast - ASCII whitespace
    #[regex("[ \t\n\r]")]
    Whitespace,
    
    // ❌ Slower - Unicode whitespace
    #[regex(r"\s")]
    UnicodeWhitespace,
    
    // ✅ Fast - specific alternatives
    #[regex("true|false")]
    Boolean,
    
    // ❌ Slower - complex alternation
    #[regex("(true|false|maybe|unknown|null|undefined)")]
    ComplexBoolean,
}
```

### Minimizing Backtracking Risk

```rust
#[derive(Logos, Debug, PartialEq)]
enum SafeToken {
    // ✅ Safe - no backtracking
    #[regex(r#""[^"]*""#)]
    SimpleString,
    
    // ⚠️ Potentially problematic - complex nested groups
    #[regex(r"((a+)+)+b")]
    NestedGroups,
    
    // ✅ Safe - possessive quantifiers (if supported)
    #[regex(r"[a-zA-Z]++")]
    PossessiveMatch,
    
    // ✅ Safe - atomic groups (if supported)
    #[regex(r"(?>pattern)")]
    AtomicGroup,
}
```

## Testing Complex Patterns

### Comprehensive Pattern Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_email_patterns() {
        let test_cases = vec![
            ("user@example.com", true),
            ("user.name@example.com", true),
            ("user+tag@example.com", true),
            ("user@sub.example.com", true),
            ("invalid.email", false),
            ("@example.com", false),
            ("user@", false),
            ("user..name@example.com", false),
        ];
        
        for (input, should_match) in test_cases {
            let mut lexer = ValidationToken::lexer(input);
            let result = lexer.next();
            
            if should_match {
                assert!(matches!(result, Some(Ok(ValidationToken::Email(_)))));
            } else {
                assert!(!matches!(result, Some(Ok(ValidationToken::Email(_)))));
            }
        }
    }
    
    #[test]
    fn test_unicode_patterns() {
        let test_cases = vec![
            ("Hello", "Latin"),
            ("Привет", "Cyrillic"),
            ("こんにちは", "Japanese"),
            ("你好", "Chinese"),
            ("🚀🌟", "Emoji"),
        ];
        
        for (input, expected_type) in test_cases {
            let mut lexer = UnicodeToken::lexer(input);
            let result = lexer.next();
            
            match expected_type {
                "Latin" => assert!(matches!(result, Some(Ok(UnicodeToken::Letters)))),
                "Cyrillic" => assert!(matches!(result, Some(Ok(UnicodeToken::Cyrillic)))),
                "Japanese" => assert!(matches!(result, Some(Ok(UnicodeToken::Japanese)))),
                "Chinese" => assert!(matches!(result, Some(Ok(UnicodeToken::Chinese)))),
                "Emoji" => assert!(matches!(result, Some(Ok(UnicodeToken::Emoji)))),
                _ => panic!("Unknown expected type: {}", expected_type),
            }
        }
    }
    
    #[test]
    fn test_string_interpolation() {
        let input = r#"`Hello ${name}, you have ${count} messages`"#;
        let mut lexer = StringToken::lexer(input);
        
        if let Some(Ok(StringToken::TemplateString(parts))) = lexer.next() {
            assert_eq!(parts.len(), 4);
            assert!(matches!(parts[0], StringPart::Text(_)));
            assert!(matches!(parts[1], StringPart::Interpolation(_)));
            assert!(matches!(parts[2], StringPart::Text(_)));
            assert!(matches!(parts[3], StringPart::Interpolation(_)));
        } else {
            panic!("Expected template string");
        }
    }
}
```

### Property-Based Testing

```rust
#[cfg(test)]
mod prop_tests {
    use super::*;
    use proptest::prelude::*;
    
    proptest! {
        #[test]
        fn email_regex_never_panics(email in "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}") {
            let mut lexer = ValidationToken::lexer(&email);
            // Should not panic
            let _ = lexer.next();
        }
        
        #[test]
        fn unicode_patterns_handle_all_input(input in ".*") {
            let mut lexer = UnicodeToken::lexer(&input);
            // Should handle any Unicode input without panicking
            while let Some(_) = lexer.next() {
                // Continue lexing
            }
        }
        
        #[test]
        fn string_patterns_are_safe(s in r#""[^"]*""#) {
            let mut lexer = StringToken::lexer(&s);
            // Should safely handle any valid string pattern
            let _ = lexer.next();
        }
    }
}
```

## Best Practices

1. **Start Simple**: Begin with basic patterns and add complexity gradually
2. **Test Thoroughly**: Use both unit tests and property-based testing
3. **Validate in Callbacks**: Use callbacks for complex validation beyond regex
4. **Consider Performance**: Profile regex patterns for performance impact
5. **Handle Unicode Carefully**: Be aware of Unicode normalization issues
6. **Document Complex Patterns**: Comment complex regex patterns for maintainability
7. **Avoid Backtracking**: Design patterns that don't require backtracking
8. **Use Character Classes**: Prefer character classes over alternation when possible

## Next Steps

- [Benchmarking and Profiling](./08-benchmarking.md) - Measure regex performance
- [API Reference](./09-api-reference.md) - Complete regex API documentation
- [Performance Optimization](./03-performance-optimization.md) - General optimization techniques