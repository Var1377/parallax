# Best Practices for Snapshot Testing

This guide covers proven patterns and recommendations for effective snapshot testing with Insta, based on real-world usage and community experience.

## General Principles

### 1. Keep Snapshots Focused and Small

**❌ Avoid large, monolithic snapshots:**
```rust
#[test]
fn test_entire_application_state() {
    let app = build_full_application();
    insta::assert_debug_snapshot!(app); // Too broad, hard to review
}
```

**✅ Create focused, specific snapshots:**
```rust
#[test]
fn test_user_profile_rendering() {
    let user = create_test_user();
    let profile = render_user_profile(&user);
    insta::assert_snapshot!(profile);
}

#[test]
fn test_user_permissions() {
    let user = create_test_user();
    let permissions = calculate_permissions(&user);
    insta::assert_yaml_snapshot!(permissions);
}
```

### 2. Use Descriptive Test Names

**❌ Generic names:**
```rust
#[test]
fn test1() { /* ... */ }

#[test]
fn test_user() { /* ... */ }
```

**✅ Descriptive names:**
```rust
#[test]
fn test_user_profile_with_all_fields_populated() { /* ... */ }

#[test]
fn test_user_profile_with_missing_optional_fields() { /* ... */ }

#[test]
fn test_user_profile_with_privacy_settings_enabled() { /* ... */ }
```

### 3. Test Both Success and Error Cases

```rust
#[test]
fn test_valid_email_parsing() {
    let email = parse_email("user@example.com").unwrap();
    insta::assert_debug_snapshot!(email);
}

#[test]
fn test_invalid_email_parsing() {
    let error = parse_email("invalid-email").unwrap_err();
    insta::assert_snapshot!(error.to_string());
}

#[test]
fn test_edge_case_email_parsing() {
    let cases = vec![
        "",
        "@",
        "user@",
        "@domain.com",
        "user@domain",
        "user.name+tag@domain.co.uk",
    ];
    
    for email in cases {
        let result = parse_email(email);
        insta::assert_debug_snapshot!(format!("email_{}", email.replace(['@', '.', '+'], "_")), result);
    }
}
```

## Organizing Tests and Snapshots

### 1. Logical Directory Structure

```
tests/
├── unit/
│   ├── parser/
│   │   ├── snapshots/
│   │   └── parser_tests.rs
│   ├── renderer/
│   │   ├── snapshots/
│   │   └── renderer_tests.rs
│   └── validation/
│       ├── snapshots/
│       └── validation_tests.rs
├── integration/
│   ├── api/
│   │   ├── snapshots/
│   │   └── api_tests.rs
│   └── e2e/
│       ├── snapshots/
│       └── e2e_tests.rs
└── fixtures/
    ├── input_data/
    └── expected_outputs/
```

### 2. Consistent Naming Conventions

```rust
// Use consistent prefixes for related tests
#[test]
fn test_parser_simple_expression() { /* ... */ }

#[test]
fn test_parser_complex_expression() { /* ... */ }

#[test]
fn test_parser_invalid_syntax() { /* ... */ }

// Group related functionality
#[test]
fn test_user_creation_with_valid_data() { /* ... */ }

#[test]
fn test_user_creation_with_invalid_email() { /* ... */ }

#[test]
fn test_user_creation_with_duplicate_username() { /* ... */ }
```

### 3. Use Modules for Organization

```rust
// tests/user_tests.rs
mod creation {
    use super::*;
    
    #[test]
    fn test_with_valid_data() {
        // Snapshots go to tests/snapshots/user_tests__creation__test_with_valid_data.snap
    }
}

mod validation {
    use super::*;
    
    #[test]
    fn test_email_validation() {
        // Snapshots go to tests/snapshots/user_tests__validation__test_email_validation.snap
    }
}
```

## Data Management

### 1. Use Test Fixtures

```rust
// tests/fixtures.rs
use serde::Serialize;

#[derive(Serialize, Clone)]
pub struct TestUser {
    pub id: u32,
    pub name: String,
    pub email: String,
    pub role: UserRole,
}

#[derive(Serialize, Clone)]
pub enum UserRole {
    Admin,
    User,
    Guest,
}

impl TestUser {
    pub fn admin() -> Self {
        Self {
            id: 1,
            name: "Admin User".to_string(),
            email: "admin@example.com".to_string(),
            role: UserRole::Admin,
        }
    }
    
    pub fn regular_user() -> Self {
        Self {
            id: 2,
            name: "Regular User".to_string(),
            email: "user@example.com".to_string(),
            role: UserRole::User,
        }
    }
    
    pub fn with_name(mut self, name: &str) -> Self {
        self.name = name.to_string();
        self
    }
}

// tests/user_tests.rs
use crate::fixtures::TestUser;

#[test]
fn test_user_serialization() {
    let user = TestUser::regular_user().with_name("Alice Johnson");
    insta::assert_yaml_snapshot!(user);
}
```

### 2. Handle Dynamic Data with Redactions

```rust
use insta::assert_yaml_snapshot;

#[test]
fn test_api_response_with_dynamic_data() {
    let response = make_api_call();
    
    assert_yaml_snapshot!(response, {
        ".timestamp" => "[timestamp]",
        ".request_id" => "[request_id]",
        ".user.id" => "[user_id]",
        ".metadata.processing_time_ms" => "[processing_time]"
    });
}
```

### 3. Create Builders for Complex Data

```rust
#[derive(serde::Serialize)]
struct Order {
    id: String,
    customer: Customer,
    items: Vec<OrderItem>,
    total: f64,
    status: OrderStatus,
}

struct OrderBuilder {
    order: Order,
}

impl OrderBuilder {
    fn new() -> Self {
        Self {
            order: Order {
                id: "order_123".to_string(),
                customer: Customer::default(),
                items: vec![],
                total: 0.0,
                status: OrderStatus::Pending,
            },
        }
    }
    
    fn with_customer(mut self, customer: Customer) -> Self {
        self.order.customer = customer;
        self
    }
    
    fn add_item(mut self, item: OrderItem) -> Self {
        self.order.total += item.price;
        self.order.items.push(item);
        self
    }
    
    fn build(self) -> Order {
        self.order
    }
}

#[test]
fn test_order_with_multiple_items() {
    let order = OrderBuilder::new()
        .with_customer(Customer::new("Alice", "alice@example.com"))
        .add_item(OrderItem::new("Widget", 29.99))
        .add_item(OrderItem::new("Gadget", 49.99))
        .build();
        
    insta::assert_yaml_snapshot!(order);
}
```

## Settings and Configuration

### 1. Use Settings for Consistency

```rust
use insta::Settings;

fn create_test_settings() -> Settings {
    let mut settings = Settings::clone_current();
    settings.set_sort_maps(true);
    settings.set_snapshot_path("custom_snapshots");
    settings.add_filter(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z", "[timestamp]");
    settings
}

#[test]
fn test_with_consistent_settings() {
    create_test_settings().bind(|| {
        let data = generate_test_data();
        insta::assert_yaml_snapshot!(data);
    });
}
```

### 2. Environment-Specific Configuration

```rust
fn setup_test_environment() -> Settings {
    let mut settings = Settings::clone_current();
    
    if std::env::var("CI").is_ok() {
        // CI-specific settings
        settings.set_sort_maps(true);
        settings.add_filter(r"build_\d+", "[build_id]");
    }
    
    if cfg!(debug_assertions) {
        // Debug-specific settings
        settings.set_description("Debug build test");
    }
    
    settings
}
```

## Testing Strategies

### 1. Layered Testing Approach

```rust
// Unit tests - test individual components
#[test]
fn test_email_validator() {
    let validator = EmailValidator::new();
    let result = validator.validate("test@example.com");
    insta::assert_debug_snapshot!(result);
}

// Integration tests - test component interactions
#[test]
fn test_user_registration_flow() {
    let service = UserService::new();
    let request = RegistrationRequest {
        email: "user@example.com".to_string(),
        password: "secure_password".to_string(),
    };
    
    let result = service.register_user(request);
    insta::assert_yaml_snapshot!(result, {
        ".user.id" => "[user_id]",
        ".user.created_at" => "[timestamp]"
    });
}

// End-to-end tests - test complete workflows
#[test]
fn test_complete_order_workflow() {
    let app = TestApp::new();
    
    // Create user
    let user = app.create_user("customer@example.com");
    
    // Add items to cart
    app.add_to_cart(&user, "product_123", 2);
    
    // Checkout
    let order = app.checkout(&user);
    
    insta::assert_yaml_snapshot!(order, {
        ".id" => "[order_id]",
        ".created_at" => "[timestamp]",
        ".user.id" => "[user_id]"
    });
}
```

### 2. Property-Based Testing with Snapshots

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_parser_properties(input in r"[a-zA-Z0-9 ]{1,50}") {
        let result = parse_input(&input);
        
        // Use snapshots for specific interesting cases
        if input.len() > 30 || input.contains("special") {
            insta::assert_debug_snapshot!(
                format!("parse_result_{}", input.len()),
                result
            );
        }
        
        // Property assertions
        match result {
            Ok(parsed) => {
                prop_assert!(!parsed.is_empty());
            }
            Err(_) => {
                // Error cases are also valid
            }
        }
    }
}
```

### 3. Regression Testing

```rust
// Keep a collection of known issues for regression testing
#[test]
fn test_regression_cases() {
    let regression_cases = vec![
        ("issue_123", "input that caused bug #123"),
        ("issue_456", "edge case from bug #456"),
        ("issue_789", "performance regression case"),
    ];
    
    for (case_name, input) in regression_cases {
        let result = process_input(input);
        insta::assert_debug_snapshot!(case_name, result);
    }
}
```

## Performance and Maintenance

### 1. Keep Snapshots Maintainable

```rust
// ❌ Hard to maintain - too much irrelevant data
#[test]
fn test_user_response() {
    let response = get_full_user_response(); // Contains 50+ fields
    insta::assert_yaml_snapshot!(response);
}

// ✅ Maintainable - focused on what matters
#[test]
fn test_user_response_essential_fields() {
    let response = get_full_user_response();
    let essential = UserResponseEssentials {
        name: response.name,
        email: response.email,
        role: response.role,
        permissions: response.permissions,
    };
    insta::assert_yaml_snapshot!(essential);
}
```

### 2. Use Helpers for Common Patterns

```rust
// Create helper functions for common redaction patterns
fn redact_user_data<T: serde::Serialize>(data: T) -> impl serde::Serialize {
    insta::with_settings!({
        redactions => vec![
            (".**.id", "[id]"),
            (".**.email", "[email]"),
            (".**.created_at", "[timestamp]"),
            (".**.updated_at", "[timestamp]"),
        ]
    }, {
        data
    })
}

#[test]
fn test_user_data() {
    let user = create_test_user();
    insta::assert_yaml_snapshot!(redact_user_data(user));
}
```

### 3. Regular Snapshot Maintenance

```rust
// Periodically review and clean up snapshots
#[test]
fn test_snapshot_health_check() {
    // This test helps identify snapshots that might need attention
    let snapshot_dir = std::path::Path::new("tests/snapshots");
    let mut large_snapshots = Vec::new();
    
    if let Ok(entries) = std::fs::read_dir(snapshot_dir) {
        for entry in entries.flatten() {
            if let Ok(metadata) = entry.metadata() {
                if metadata.len() > 10_000 { // 10KB threshold
                    large_snapshots.push(entry.file_name().to_string_lossy().to_string());
                }
            }
        }
    }
    
    if !large_snapshots.is_empty() {
        insta::assert_yaml_snapshot!("large_snapshots_review", large_snapshots);
    }
}
```

## Error Handling and Debugging

### 1. Meaningful Error Snapshots

```rust
#[test]
fn test_validation_errors() {
    let invalid_data = InvalidUserData {
        email: "not-an-email",
        age: -5,
        name: "",
    };
    
    let errors = validate_user_data(invalid_data);
    
    // Create structured error snapshot
    let error_summary = errors.iter().map(|e| {
        serde_json::json!({
            "field": e.field,
            "code": e.error_code,
            "message": e.message,
            "severity": e.severity
        })
    }).collect::<Vec<_>>();
    
    insta::assert_json_snapshot!(error_summary);
}
```

### 2. Debug Information in Snapshots

```rust
#[test]
fn test_complex_calculation() {
    let input = ComplexInput::new();
    let result = perform_complex_calculation(&input);
    
    // Include debug information in snapshot
    let debug_info = serde_json::json!({
        "input_summary": input.summarize(),
        "calculation_steps": result.steps,
        "final_result": result.value,
        "performance_metrics": {
            "duration_ms": result.duration.as_millis(),
            "memory_used": result.memory_used,
        }
    });
    
    insta::assert_json_snapshot!(debug_info);
}
```

### 3. Conditional Debug Snapshots

```rust
#[test]
fn test_with_optional_debug() {
    let result = complex_operation();
    
    // Always snapshot the main result
    insta::assert_yaml_snapshot!("main_result", result.output);
    
    // Optionally snapshot debug info
    if std::env::var("SNAPSHOT_DEBUG").is_ok() {
        insta::assert_yaml_snapshot!("debug_info", result.debug_info);
    }
}
```

## Team Collaboration

### 1. Document Snapshot Expectations

```rust
/// Tests the user authentication flow.
/// 
/// This test verifies that:
/// - Valid credentials return a success response with user data
/// - Invalid credentials return appropriate error messages
/// - Rate limiting is applied correctly
/// 
/// Snapshots include:
/// - User data (with sensitive fields redacted)
/// - Error responses for various failure modes
/// - Rate limiting headers and status
#[test]
fn test_user_authentication() {
    // Test implementation...
}
```

### 2. Consistent Review Process

```bash
# Create a script for snapshot review
#!/bin/bash
# scripts/review-snapshots.sh

echo "Reviewing snapshot changes..."
cargo insta review

echo "Checking for large snapshots..."
find tests/snapshots -name "*.snap" -size +10k -exec ls -lh {} \;

echo "Running tests to verify snapshots..."
cargo test
```

### 3. Snapshot Guidelines for Teams

```markdown
# Team Snapshot Testing Guidelines

## Before Committing Snapshots:
1. Review each snapshot change carefully
2. Ensure snapshots are focused and readable
3. Verify that dynamic data is properly redacted
4. Check that snapshot names are descriptive

## When Updating Snapshots:
1. Understand why the snapshot changed
2. Verify the change is intentional
3. Update related documentation if needed
4. Consider if the change indicates a breaking change

## Snapshot Size Guidelines:
- Keep individual snapshots under 5KB when possible
- Break large snapshots into smaller, focused ones
- Use redactions to hide irrelevant data
- Consider using summaries for very large data structures
```

## Common Pitfalls and Solutions

### 1. Avoid Non-Deterministic Data

```rust
// ❌ Non-deterministic - will fail randomly
#[test]
fn test_with_random_data() {
    let data = generate_random_data();
    insta::assert_debug_snapshot!(data); // Will change every run
}

// ✅ Deterministic - use fixed seeds or mock data
#[test]
fn test_with_deterministic_data() {
    let mut rng = StdRng::seed_from_u64(42); // Fixed seed
    let data = generate_data_with_rng(&mut rng);
    insta::assert_debug_snapshot!(data);
}
```

### 2. Handle Platform Differences

```rust
#[test]
fn test_cross_platform() {
    let result = platform_specific_operation();
    
    // Normalize platform-specific data
    let normalized = normalize_platform_data(result);
    insta::assert_yaml_snapshot!(normalized);
}

fn normalize_platform_data(data: PlatformData) -> NormalizedData {
    NormalizedData {
        // Convert platform-specific paths to generic format
        path: data.path.to_string_lossy().replace('\\', "/"),
        // Normalize line endings
        content: data.content.replace("\r\n", "\n"),
        // Other normalizations...
    }
}
```

### 3. Manage Snapshot Dependencies

```rust
// ❌ Snapshots that depend on external state
#[test]
fn test_with_external_dependency() {
    let data = fetch_from_external_api(); // Unreliable
    insta::assert_json_snapshot!(data);
}

// ✅ Use mocks or fixtures
#[test]
fn test_with_mocked_data() {
    let mock_data = load_test_fixture("api_response.json");
    let processed = process_api_response(mock_data);
    insta::assert_json_snapshot!(processed);
}
```

This comprehensive guide should help you implement effective snapshot testing practices with Insta. Remember that good snapshot tests are focused, maintainable, and provide clear value in catching regressions and documenting expected behavior.